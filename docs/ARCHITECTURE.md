# アーキテクチャとデータパイプライン

本ツールキットの設計思想、データの流れ、共通データ構造を説明します。

---

## 1. 全体設計

### 1.1 三層（MVC）構成

各分析モジュールは原則として 3 つのファイルに分割されます。

| サフィックス | 層 | 責務 | 禁止事項 |
|---|---|---|---|
| `*_calculator.R` | Model | 統計量の計算のみ。プレーンな R オブジェクト（list / data.frame）を返す | 表示・ファイル I/O・設定読込をしない |
| `*_display.R` | View | `cat` / `print` / ggplot による表示のみ | 計算をしない（計算済みオブジェクトを受け取る） |
| `*_main.R` | Controller | 設定読込 → データ抽出 → calculator 呼出 → display 呼出 → 返却 | ユーザーが直接呼ぶ「公開関数」を定義 |

この分離により、計算ロジックを表示と独立してテスト・再利用でき、`analyze_*()`（計算＋全表示）と `show_*_evaluation()`（評価判定のみ表示）のような複数のエントリポイントを同じ calculator から構築できます。

### 1.2 設定駆動

- すべての可変パラメータ（データセット、尺度範囲、因子構造、モデル定義、しきい値）は YAML に集約。
- 設定の読込と検証は `config_loader.R::load_config()` に一元化。
- `config$system`（= `system_config.yaml`）と `config$analysis`（= `analysis_config.yaml`）の 2 系統。
- ほとんどの calculator / display は **設定を読まず**、必要な値は引数で受け取る。設定アクセスは `*_main.R`（Controller）に閉じている。

---

## 2. データパイプライン

```
system_config.yaml ─┐
analysis_config.yaml ┴─► load_config() ──► CONFIG (list: $system, $analysis)
                                                │
                                                ▼
                              connect_sheets()  ─── OAuth (googlesheets4)
                                                │
                                                ▼
                              get_sheets_data(dataset_name)  ── read_sheet()
                                                │
                                                ▼
                        ┌──────── prepare_data() ────────┐
                        │  target  : 解析対象尺度          │
                        │  rehab   : 基準尺度(REHAB)       │
                        │  interrater : 評価者間データ      │
                        └────────────────────────────────┘
                                                │
                                  各列の型変換・キー付与
                                                │
                                                ▼
                    list(target=, rehab=, interrater=, config=)
                          各要素は keyed_data オブジェクト
```

### 2.1 `load_config()`（`config_loader.R`）

- `system_config.yaml` と `analysis_config.yaml` を読み込み、`CONFIG$system` / `CONFIG$analysis` に格納。
- 必須項目を検証（無ければ `stop()`）:
  - `system$google_sheets`
  - `system$authentication$email_auth`
  - `analysis$global$scale`

### 2.2 `connect_sheets()` / `get_sheets_data()`（`google_sheets_connector.R`）

- `gs4_auth(email=..., cache=...)` で OAuth 認証。トークンは `.secrets/` にキャッシュ。
- `get_sheets_data(dataset_name)` は `system$google_sheets[[dataset_name]]` の `spreadsheet_id` / `sheet_name` を使って `read_sheet()` で取得。

### 2.3 `prepare_data()`（`prepare_data.R`）

統合エントリポイント。以下を実行:

1. `load_config()` → `connect_sheets()`。
2. `analysis$data_source$dataset` で指定された target データセットを取得。
3. `rehab`・`interrater` データセットを取得。
4. **型変換**（`clean_data()` / `convert_q_columns()`）:
   - target: `^Q\d{2}$` 列を数値化。
   - rehab: `^R_Q\d{2}$` 列を数値化。
   - interrater: 患者 ID 列を残しつつ `^Q\d{2}$` 列を数値化。
5. **行名（患者 ID）の付与** — `data_keys` で指定したキー列を `rownames` に設定。
6. target ↔ rehab の ID 一致確認、interrater の「2 評価」ペア数を確認。
7. 返り値:

```r
list(
  target     = keyed_data,   # 解析対象（行=患者, 列=Q項目）
  rehab      = keyed_data,   # REHAB（行=患者, 列=R_Q項目）
  interrater = keyed_data,   # 評価者間（患者IDを含む積み上げ形式）
  config     = CONFIG
)
```

### 2.4 データクリーニング（`data_cleaner.R`）

- `flatten_list_columns()` — `read_sheet()` が返すリスト列をベクトルに平坦化。
- `convert_q_columns(data, item_pattern)` — パターン一致列を factor/character/logical から数値へ変換。
- `clean_data(raw, item_pattern, remove_na_rows)` — 平坦化 → 数値化 → パターン一致列のみ抽出 →（任意で）完全ケースのみ残す。

### 2.5 因子分析用前処理（`data_preprocessor.R`）

`preprocess_for_fa(data, method, item_pattern, scale_min, scale_max)`:

- Q 列のみ抽出。
- 欠測処理（`listwise` = 完全ケース / `pairwise` = 列ごとの欠測報告のみ）。
- **順序尺度の範囲検証** — `scale_min`〜`scale_max` の整数外があれば `stop()`。
- ケース:変数比を表示。

---

## 3. 共通データ構造: `keyed_data`（`data_structure.R`）

ID キーと本体データを一体で扱う軽量 S3 クラス。

```r
create_data_with_keys(keys, data)   # keys=character vector, data=data.frame
#  -> structure(list(keys=keys, data=data), class=c("keyed_data","list"))

get_data(obj)   # 本体 data.frame を取り出す
get_keys(obj)   # キー（ID 列名）を取り出す
has_keys(obj)   # キーを持つか判定
```

すべての公開分析関数は `keyed_data`（または `data` / `keys` を持つ list）を受け取り、内部で `get_data()` により項目データを取り出します。`create_clean_dataset()` などはフィルタ後に再度 `create_data_with_keys()` で包み直すため、ID 情報を保ったまま下流分析へ渡せます。

---

## 4. 感度分析の共通基盤（`sensitivity_utils.R`）

`generate_exclude_subsets(exclude_items)` は除外項目の **冪集合（power-set）** を生成します。

```r
generate_exclude_subsets(c("Q19","Q23"))
# -> full, excl_Q19, excl_Q23, excl_Q19_Q23   （4 サブセット）

generate_exclude_subsets(character(0))
# -> full のみ（感度分析オフ）
```

因子数決定（`factor_number_main.R`）、EFA（`efa_main.R`）、評価者間信頼性（`interrater_main.R`）が、それぞれの `sensitivity_exclude_items` 設定をこの関数に渡し、各サブセットで分析を再実行します。

---

## 5. ロード順序（`load_scripts.R`）

`load_scripts.R` は依存関係順に全ファイルを `source()` します。

1. ライブラリ読込（`suppressPackageStartupMessages`）
2. 基盤: `config_loader` → `data_structure` → `google_sheets_connector` → `data_cleaner` → `prepare_data`
3. 各分析モジュール（calculator → display → main の順）
4. 末尾でコピー&ペースト用の使用例をコンソール出力

実行後は `prepare_data()` を起点に各 `analyze_*()` を呼び出します。

---

## 6. 統計手法で共通する設計判断

- **polychoric と Pearson の併用** — 4 件法の順序性を踏まえ、項目間相関・因子分析適合性・因子数決定・EFA では両方を計算して併記。Pearson 解は polychoric 解に整列（factor congruence + ハンガリアン法 `clue::solve_LSAP`）して比較。
- **WLSMV 推定** — CFA / ESEM では順序カテゴリカル指標として `lavaan` の WLSMV・robust SE・pairwise 欠測を使用。
- **しきい値の所在** — 一部の評価しきい値は YAML（例: `efa_evaluation`, `factor_number`）にあるが、GP・IT・天井床のしきい値はコード内にハードコードされている（詳細は [ANALYSES.md](ANALYSES.md)）。
