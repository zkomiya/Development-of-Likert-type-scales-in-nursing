# 看護領域における Likert 型尺度の開発・検証ツールキット

> Development of Likert-type Scales in Nursing — an R toolkit for psychometric scale development and validation.

精神科看護領域で開発中の患者用 Likert 尺度（4 件法）を、**項目分析 → 信頼性 → 因子構造（探索 / 確認）→ 妥当性 → 評価者間信頼性**まで一気通貫で評価するための R スクリプト群です。データは Google スプレッドシートから読み込み、すべての分析パラメータは YAML 設定ファイルで制御します。

- 詳細な設定リファレンス: [docs/CONFIGURATION.md](docs/CONFIGURATION.md)
- 分析モジュール API リファレンス: [docs/ANALYSES.md](docs/ANALYSES.md)
- アーキテクチャとデータパイプライン: [docs/ARCHITECTURE.md](docs/ARCHITECTURE.md)

---

## 目次

- [特徴](#特徴)
- [必要環境](#必要環境)
- [セットアップ](#セットアップ)
- [クイックスタート](#クイックスタート)
- [標準的な分析ワークフロー](#標準的な分析ワークフロー)
- [分析モジュール一覧](#分析モジュール一覧)
- [設定ファイル](#設定ファイル)
- [出力](#出力)
- [プロジェクト構成](#プロジェクト構成)
- [既知の制約・注意点](#既知の制約注意点)

---

## 特徴

- **Google Sheets 連携** — `googlesheets4` による OAuth 認証で本番・テストデータを直接読み込み。
- **設定駆動** — データセット選択、尺度範囲、因子構造、CFA/ESEM モデル、各種しきい値をすべて `analysis_config.yaml` / `system_config.yaml` で管理。コードを書き換えずに分析対象を切り替え可能。
- **三層アーキテクチャ** — 各分析は `*_calculator.R`（計算）/ `*_display.R`（表示）/ `*_main.R`（制御）に分離（MVC 構成）。
- **ポリコリック相関の併用** — 4 件法の順序尺度を前提に、多くの分析で polychoric 相関と Pearson 相関を並行計算。
- **感度分析（sensitivity analysis）** — 指定項目を除外した全組み合わせ（power-set）で因子数決定・EFA・評価者間信頼性を再実行。
- **網羅的な手法** — Cronbach's α / McDonald's ω、平行分析・MAP・Kaiser、EFA（複数抽出法 × 因子数 × 回転のグリッド）、WLSMV による CFA / ESEM と尤度比検定、Steiger 検定による収束・弁別妥当性、重み付き κ / Gwet's AC2 / ICC / Bland-Altman。

---

## 必要環境

- **R**（4.x 推奨）
- Google アカウント（スプレッドシートへの閲覧権限と OAuth 認証）

### 依存パッケージ

`load_scripts.R` の冒頭で読み込むもの:

```
googlesheets4, tidyverse, yaml, pwr, psych, polycor, GPArotation,
clue, irr, boot, BlandAltmanLeh, irrCAC, lavaan, effectsize, openxlsx, careless
```

加えて、一部モジュールが内部で利用するもの:

```
EFA.MRFA   # 因子数決定（parallelMRFA）
gridExtra, scales   # 可視化
Matrix     # 行列の特異性診断（任意。無ければ代替計算）
```

一括インストール例:

```r
install.packages(c(
  "googlesheets4", "tidyverse", "yaml", "pwr", "psych", "polycor",
  "GPArotation", "clue", "irr", "boot", "BlandAltmanLeh", "irrCAC",
  "lavaan", "effectsize", "openxlsx", "careless", "EFA.MRFA",
  "gridExtra", "scales", "Matrix"
))
```

---

## セットアップ

1. **リポジトリを取得**（クローン済みであれば不要）。

2. **`system_config.yaml` を用意する。**
   このファイルは `.gitignore` 済みで、スプレッドシート ID と認証メールを含みます。リポジトリにはサンプルが含まれていますが、自分の環境では以下を確認・修正してください。

   ```yaml
   authentication:
     cache_folder: ".secrets"
     email_auth: "あなたのGoogleアカウント@example.com"

   google_sheets:
     items_30-Q14-Q19:
       spreadsheet_id: "<スプレッドシートID>"
       sheet_name: "patients_data"
     rehab:
       spreadsheet_id: "<REHABデータのID>"
       sheet_name: "patients_data"
     interrater:
       spreadsheet_id: "<評価者間データのID>"
       sheet_name: "inter-rater_data"
   ```

3. **`analysis_config.yaml` で分析対象を選ぶ。**

   ```yaml
   data_source:
     dataset: items_30-Q14-Q19   # ここで解析対象データセットを指定
   ```

4. **R を作業ディレクトリで起動**し、全スクリプトを読み込む:

   ```r
   source("load_scripts.R")
   ```

   読み込み後、コピー&ペースト用の使用例がコンソールに表示されます。

> **認証について**: 初回実行時に `connect_sheets()`（`prepare_data()` 内で自動呼び出し）がブラウザ経由の OAuth を要求します。トークンは `.secrets/` にキャッシュされます（`.gitignore` 済み）。

---

## クイックスタート

```r
source("load_scripts.R")

# 1. データ読み込み（target / rehab / interrater をまとめて取得）
data <- prepare_data()

# 2. 回答の質スクリーニング（任意。手抜き回答・回答スタイルの除外）
sat_results <- analyze_satisficing(data)
clean_data  <- create_clean_dataset(data$target, sat_results)

# 3. 項目分析
analyze_gp(data$target)               # G-P 分析（識別力）
analyze_item_correlations(data$target) # 項目間相関
analyze_item_total(data$target)        # 項目-合計相関
analyze_ceiling_floor(data$target)     # 天井・床効果

# 4. 信頼性
analyze_reliability(data$target)       # α と ω（下位尺度別＋全体）

# 5. 因子分析
check_fa_suitability(data$target)      # KMO / Bartlett
determine_factors(data$target)         # 因子数の決定
analyze_efa(data$target)               # 探索的因子分析
analyze_cfa(data$target, "icm_cfa")    # 確認的因子分析
compare_cfa_models(data$target)        # モデル比較＋尤度比検定

# 6. ESEM
analyze_esem(data$target, "five_factor_model")
compare_cfa_esem(data$target, "icm_cfa")

# 7. 妥当性（REHAB 尺度との収束・弁別妥当性）
analyze_validity(data$target, data$rehab)

# 8. 評価者間信頼性
analyze_interrater(data$interrater)
```

---

## 標準的な分析ワークフロー

```
prepare_data()                データ取得（Google Sheets → keyed_data）
        │
        ▼
[回答品質スクリーニング]   analyze_satisficing / analyze_response_styles
        │                  → create_clean_dataset で除外
        ▼
[項目分析]                 GP / 項目間相関 / 項目-合計相関 / 天井・床
        │
        ▼
[信頼性]                   Cronbach α・McDonald ω
        │
        ▼
[因子分析の前提確認]        KMO・Bartlett・特異性診断
        │
        ▼
[因子数の決定]             平行分析(MRFA/FA/PCA)・MAP・Kaiser
        │
        ▼
[探索的因子分析 EFA]       抽出法 × 因子数 × 回転のグリッド探索
        │
        ▼
[確認的因子分析 CFA/ESEM]   WLSMV 推定・適合度・尤度比検定
        │
        ▼
[妥当性]                   REHAB 尺度との収束・弁別妥当性（Steiger 検定）
        │
        ▼
[評価者間信頼性]            重み付き κ・Gwet AC2・ICC・Bland-Altman
```

各関数の引数・出力・評価しきい値は [docs/ANALYSES.md](docs/ANALYSES.md) を参照してください。

---

## 分析モジュール一覧

| カテゴリ | 主な関数 | 内容 |
|---|---|---|
| **データ準備** | `prepare_data()` | Google Sheets から target / rehab / interrater を読み込み、型変換・キー付与 |
| **回答品質** | `analyze_satisficing()`, `analyze_response_styles()` | 手抜き回答（longstring / IRV / Mahalanobis）と回答スタイル（ERS / MRS）の検出 |
| **項目分析** | `analyze_gp()` | Good-Poor 分析（上位・下位 27% の識別力、D\*・Cohen's d） |
| | `analyze_item_correlations()` | 項目間相関（polychoric / Pearson）、平均項目間相関 MIIC |
| | `analyze_item_total()` | 項目-合計相関（Pearson / polyserial、修正済み I-T） |
| | `analyze_ceiling_floor()` | 天井・床効果、歪度・尖度 |
| **信頼性** | `analyze_reliability()` | Cronbach's α と McDonald's ω（全体＋下位尺度） |
| **因子分析** | `check_fa_suitability()` | KMO・Bartlett 球面性検定・行列特異性診断 |
| | `determine_factors()` | 平行分析（MRFA / FA / PCA）・MAP・Kaiser による因子数決定 |
| | `analyze_efa()` | 探索的因子分析（複数抽出法 × 因子数 × oblimin/promax のグリッド） |
| | `analyze_cfa()`, `compare_cfa_models()` | 確認的因子分析（WLSMV）とネストモデルの尤度比検定 |
| | `analyze_esem()`, `compare_cfa_esem()` | ESEM（geomin 回転）と CFA との比較 |
| **妥当性** | `analyze_validity()` | REHAB 尺度を基準とした収束・弁別妥当性（H1〜H6、Steiger 検定） |
| **評価者間** | `analyze_interrater()` | 重み付き κ・Gwet's AC2・ICC(2,1)/(2,2)・Bland-Altman |
| **可視化** | `visualize_data()` | 項目別回答分布・合計得点分布・相関ヒートマップ（ggplot2） |

---

## 設定ファイル

| ファイル | 役割 | Git 管理 |
|---|---|---|
| `system_config.yaml` | スプレッドシート ID、シート名、OAuth 認証メール、キャッシュフォルダ | **対象外**（`.gitignore`） |
| `analysis_config.yaml` | 解析対象データセット、尺度範囲、項目パターン、下位尺度/因子定義、CFA/ESEM モデル、各分析のしきい値 | 管理対象 |

主要な設定項目（抜粋）:

- `data_source.dataset` — 解析対象データセット名
- `global.scale.min` / `global.scale.max` — 尺度の下限・上限（現状 1〜4）
- `global.item_pattern` — 項目列の正規表現（`^Q\d{2}$`）
- `scale_structure.<dataset>` — データセットごとの因子・下位尺度定義（項目割り当て）
- `efa_settings` — 抽出法・因子数候補・回転・感度分析対象項目
- `cfa_models.<dataset>` / `esem_models.<dataset>` — モデル構文・表示名・`lrt_chain`
- `validity_hypotheses` — 妥当性仮説 H1〜H6（target / proximal / distal）
- `interrater_analysis` — κ の重み、ブートストラップ反復数、AC2 の重み、除外項目

全項目の詳細は [docs/CONFIGURATION.md](docs/CONFIGURATION.md) を参照。

---

## 出力

- **CSV ファイル**
  - `output/` — 項目分析・信頼性などの結果（例: `gp_analysis_results.csv`, `it_correlations.csv`, `ceiling_floor_results.csv`）
  - `efa_output/` — EFA のパターン行列（例: `pattern_<dataset>_<抽出法>_<回転>_<因子数>_<相関型>.csv`）
  - いずれも `.gitignore` 済み
- **コンソール出力** — すべての分析は整形済みのテーブルと評価判定をコンソールに表示
- **プロット** — `visualize_data()` はアクティブなグラフィックスデバイスに描画（ファイル保存はしない）

`output.txt` はリポジトリに含まれる過去の EFA 実行ログ（30 項目・396 → 365 ケース、感度分析 4 サブセット）のキャプチャです。

---

## プロジェクト構成

```
.
├── load_scripts.R            # 全スクリプトの読み込み＋使用例の表示（エントリポイント）
├── config_loader.R           # YAML 設定の読み込み・検証
├── system_config.yaml        # データソース・認証（.gitignore）
├── analysis_config.yaml      # 分析パラメータ
│
├── google_sheets_connector.R # Google Sheets 接続・取得
├── data_structure.R          # keyed_data 構造（keys + data）
├── data_cleaner.R            # 型変換・列抽出・NA 処理
├── data_preprocessor.R       # 因子分析用の前処理（欠測・範囲検証）
├── prepare_data.R            # データ準備の統合関数
├── sensitivity_utils.R       # 除外項目の組み合わせ生成（power-set）
│
├── satisficing_*.R           # 手抜き回答の検出
├── response_styles_*.R       # 回答スタイル（ERS/MRS）
├── gp_*.R                    # Good-Poor 分析
├── ii_*.R                    # 項目間相関
├── it_*.R                    # 項目-合計相関
├── ceiling_floor_*.R         # 天井・床効果
├── alpha_*.R / omega_*.R     # 信頼性係数
├── reliability_main.R        # 信頼性の統合
├── factor_prerequisites.R    # 因子分析の前提（KMO/Bartlett ヘルパ）
├── factor_suitability_*.R    # 因子分析適合性
├── factor_number_*.R         # 因子数決定
├── efa_*.R                   # 探索的因子分析
├── lavaan_utils_*.R          # lavaan 共通（適合度・尤度比など）
├── cfa_*.R                   # 確認的因子分析
├── esem_*.R                  # ESEM
├── validity_*.R              # 妥当性
├── interrater_*.R            # 評価者間信頼性
├── visualization_*.R         # 可視化
│
├── output.txt                # 過去の EFA 実行ログ
└── docs/                     # 詳細ドキュメント
    ├── ARCHITECTURE.md
    ├── CONFIGURATION.md
    └── ANALYSES.md
```

各分析モジュールは原則 3 ファイル構成です:

| サフィックス | 役割 |
|---|---|
| `*_calculator.R` | 純粋な計算（統計量の算出のみ。表示・I/O を持たない） |
| `*_display.R` | 表示（`cat` / `print` / ggplot のみ。計算しない） |
| `*_main.R` | 制御（設定読込 → 計算 → 表示 → 返り値。ユーザーが呼ぶ公開関数） |

---

## 既知の制約・注意点

- **`system_config.yaml` は機密**: スプレッドシート ID と認証メールを含み、`.gitignore` 済みです。リポジトリ内のサンプルには実 ID が含まれているため、公開時は取り扱いに注意してください。
- **`show_ii_evaluation()` の潜在バグ**: `ii_main.R` の下位尺度評価で `calculate_subscale_correlations(cor_poly, subscale_def)` が必須引数 `n_obs` を渡しておらず（定義は `function(cor_matrix, subscale_def, n_obs)`）、この経路を実行するとエラーになります。
- **`analyze_validity()` の引数**: 実際のシグネチャは `analyze_validity(target_obj, rehab_obj)` です。
- **`create_rs_clean_dataset()` の引数**: `create_rs_clean_dataset(data_obj, rs_results, dataset = "target", exclude_type = "ers")`。除外タイプは `"ers"` / `"mrs"` / `"both"` のいずれか。
- **ESEM / CFA のモデル定義はデータセット依存**: `compare_cfa_esem()` を使うには、対象データセットに対し同名モデルが `cfa_models` と `esem_models` の双方に定義されている必要があります。
- **回答スタイルは ERS / MRS のみ**: 黙従反応（ARS, acquiescence）は未実装です。
- **信頼性係数に閾値判定なし**: α・ω は数値のみ表示され、`≥ .70` などの自動判定は行いません（解釈は利用者に委ねられます）。

---

詳細は `docs/` 以下を参照してください。改善要望・不具合があれば Issue へ。
