# 設定リファレンス

すべての分析パラメータは 2 つの YAML ファイルで制御されます。本ドキュメントは各キーの意味・現在値・利用箇所をまとめます。

- `system_config.yaml` — データソースと認証（`.gitignore` 済み、機密）
- `analysis_config.yaml` — 分析パラメータ（Git 管理対象）

読込と検証は `config_loader.R::load_config()` が担当し、結果は `CONFIG$system` / `CONFIG$analysis` に格納されます。

---

## 1. system_config.yaml

スプレッドシートのマッピングと OAuth 認証情報。

```yaml
google_sheets:
  <dataset_name>:
    spreadsheet_id: "<Google スプレッドシート ID>"
    sheet_name: "<シート名>"
  # ... 複数定義可能（テストデータ・本番データ）

authentication:
  cache_folder: ".secrets"          # OAuth トークンのキャッシュ先
  email_auth: "you@example.com"     # 認証に使う Google アカウント
```

| キー | 説明 |
|---|---|
| `google_sheets.<name>.spreadsheet_id` | 各データセットのスプレッドシート ID |
| `google_sheets.<name>.sheet_name` | 読み込むシート名 |
| `authentication.cache_folder` | `gargle` の OAuth キャッシュフォルダ（`.secrets`） |
| `authentication.email_auth` | OAuth に使用するメールアドレス |

定義済みデータセット名は本番（`items_30`, `items_29`, `items_28`, `items_30-Q14-Q19`, `items_30-Q19`, `items_28-Q07`, `rehab`, `interrater`）とテスト（`satisficing_test_data`, `efa_unit_test_*`）。

> **重要**: このファイルは実 ID と認証メールを含むため `.gitignore` 済みです。

---

## 2. analysis_config.yaml

### 2.1 データソース

```yaml
data_source:
  dataset: items_30-Q14-Q19   # 解析対象データセット（system_config の名前を指定）
```

| キー | 利用箇所 |
|---|---|
| `data_source.dataset` | `prepare_data()` が読み込む target データセットの選択。多くの分析が `scale_structure[[dataset]]` 等をこの名前で参照 |

### 2.2 グローバル設定

```yaml
global:
  scale:
    min: 1
    max: 4
  item_pattern: "^Q\\d{2}$"
```

| キー | 値 | 利用箇所 |
|---|---|---|
| `global.scale.min` / `max` | 1 / 4 | 尺度範囲。天井・床効果、回答スタイル、前処理の範囲検証、可視化など |
| `global.item_pattern` | `^Q\d{2}$` | 項目列の判定（Q01〜Q99） |

### 2.3 データキー

```yaml
data_keys:
  items_30-Q14-Q19: "患者ID"
  rehab: "患者ID"
  interrater: "患者ID"
  # ...
```

各データセットの ID 列名。`prepare_data()` が `rownames` 設定に、`interrater` が患者ペアの構成に使用。

### 2.4 回答品質スクリーニング

```yaml
satisficing:
  longstring:
    threshold: 10            # 連続同一回答がこの値を超えると flag
  irv:
    percentile_threshold: 5  # IRV 下位 5% を flag（変動が小さすぎる）
  mahalanobis:
    p_threshold: 0.001       # Mahalanobis 距離の p がこの値未満で flag

response_styles:
  extreme_percentile: 95     # ERS/MRS 上位 5%（95 パーセンタイル超）を flag
```

| キー | 利用 |
|---|---|
| `satisficing.longstring.threshold` | `careless::longstring` の判定（`> threshold`） |
| `satisficing.irv.percentile_threshold` | IRV のパーセンタイル閾値（`< quantile`） |
| `satisficing.mahalanobis.p_threshold` | Mahalanobis 距離の χ² p 値閾値 |
| `response_styles.extreme_percentile` | ERS・MRS の上側パーセンタイル閾値 |

### 2.5 GP 分析・相関

```yaml
gp_analysis:
  cutoff_percentile: 0.27    # 上位・下位 27% で群分け

correlation_analysis:
  use: "pairwise.complete.obs"
```

| キー | 利用 |
|---|---|
| `gp_analysis.cutoff_percentile` | GP 分析の上位/下位群カットオフ |
| `correlation_analysis.use` | `cor()` の欠測処理（項目間相関・可視化） |

### 2.6 尺度構造（下位尺度・因子定義）

```yaml
scale_structure:
  <dataset>:
    <factor_key>:
      name: "<日本語表示名>"
      items: ["Q01", "Q02", ...]
```

データセットごとに因子（下位尺度）と所属項目を定義。**項目間相関・項目-合計相関・信頼性（α/ω の下位尺度）・妥当性**で参照されます。

現行 `items_30-Q14-Q19` の 5 因子:

| キー | 表示名 | 項目数 |
|---|---|---|
| factor_1 | 精神生活の安定・自己調整 | Q10, Q12, Q15, Q16 |
| factor_2 | 疾患・治療理解 | Q01, Q02, Q03 |
| factor_3 | 退院準備への主体性・援助希求 | Q07, Q11, Q13, Q24, Q25, Q30 |
| factor_4 | 生活管理・地域生活技能 | Q04, Q05, Q06, Q08, Q09, Q17, Q18, Q20, Q21, Q22, Q23 |
| factor_5 | 現時点の危険行動の不在 | Q26, Q27, Q28, Q29 |

### 2.7 因子数決定

```yaml
factor_number:
  parallel_analysis_iterations: 1000
  percentile: 95
  sensitivity_exclude_items: []
```

| キー | 利用 |
|---|---|
| `factor_number.parallel_analysis_iterations` | 平行分析の反復回数 |
| `factor_number.percentile` | 平行分析のパーセンタイル基準 |
| `factor_number.sensitivity_exclude_items` | 感度分析の除外項目（power-set 生成。空なら full のみ） |

### 2.8 EFA 設定

```yaml
efa_settings:
  missing: "listwise"                       # listwise / pairwise
  pd_tolerance: 1.0e-6                       # 正定値性の許容誤差
  extraction_methods: ["wls","uls","ml","pa","minres"]
  n_factors_list: [2,4,5,6]
  gamma_values: [0.0]                        # oblimin の gamma
  kaiser_normalize: true
  max_iterations: 1000
  flip_factors: true                         # 因子符号の自動反転
  promax_kappa_values: [4]                   # promax の kappa
  display_cutoff: null                       # 表示時の負荷量抑制閾値（null=全表示）
  sensitivity_exclude_items: []

efa_evaluation:
  primary_threshold: 0.32   # 主負荷量の下限
  cross_threshold: 0.2      # 二次負荷量がこれ以上で交差負荷
  diff_threshold: 0.1       # 主-二次の差がこれ以下で交差負荷
```

EFA はこれらの **抽出法 × 因子数 × 回転（gamma/kappa）× 相関型（poly/Pearson）** のグリッドを総当たりします。`efa_evaluation` は項目の良否（主負荷不足・交差負荷）判定に使用。

### 2.9 REHAB 下位尺度

```yaml
rehab_subscales:
  deviant_behavior:
    name: "逸脱行動"
    items: ["R_Q01", ... "R_Q07"]
  general_behavior:
    rehab_gb_factor_1:
      name: "社会的活動性"
      items: ["R_Q08", ...]
    # ... factor_2 〜 factor_6
```

妥当性分析で基準となる REHAB 尺度の下位尺度定義。

### 2.10 妥当性仮説

```yaml
validity_hypotheses:
  H1:
    target: factor_4            # 解析対象尺度の因子
    proximal: rehab_gb_factor_4 # 近接（収束を予測）
    distal: deviant_behavior    # 遠隔（弁別を予測）
  # H2 〜 H6
```

各仮説は「target と proximal の相関 > target と distal の相関」を予測。Steiger（1980）の従属相関差検定（片側）で検証されます。

| 仮説 | target | proximal | distal |
|---|---|---|---|
| H1 | factor_4 | rehab_gb_factor_4（社会生活の技能） | deviant_behavior（逸脱行動） |
| H2 | factor_4 | rehab_gb_factor_3（セルフケア） | deviant_behavior |
| H3 | factor_4 | rehab_gb_factor_4 | rehab_gb_factor_2（ことばのわかりやすさ） |
| H4 | factor_5 | deviant_behavior | rehab_gb_factor_4 |
| H5 | factor_5 | deviant_behavior | rehab_gb_factor_3 |
| H6 | factor_3 | rehab_gb_factor_1（社会的活動性） | deviant_behavior |

### 2.11 評価者間信頼性

```yaml
interrater_analysis:
  kappa_weight: "squared"
  bootstrap_iterations: 1000
  ac2_weights: "quadratic"
  sensitivity_exclude_items: ["Q19"]
```

| キー | 利用 |
|---|---|
| `interrater_analysis.kappa_weight` | （注）コードでは `kappa2(weight="squared")` がハードコードのため現状は装飾的 |
| `interrater_analysis.bootstrap_iterations` | κ のブートストラップ CI 反復数 |
| `interrater_analysis.ac2_weights` | Gwet's AC2 の重み（quadratic=AC2 / unweighted=AC1） |
| `interrater_analysis.sensitivity_exclude_items` | 感度分析の除外項目（現状 `["Q19"]` → full と excl_Q19） |

### 2.12 CFA 設定・モデル

```yaml
cfa_settings:
  estimator: "WLSMV"
  missing: "pairwise"
  se: "robust"

cfa_models:
  <dataset>:
    lrt_chain: ["icm_cfa", "rc_q20_q21", ...]   # 尤度比検定の順序（制約強→弱）
    <model_name>:
      name: "<表示名>"
      display_names: { factor_1: "...", ... }   # 因子キー→表示名（YAML アンカー利用可）
      model_syntax: |
        factor_1 =~ Q10 + Q12 + Q15 + Q16
        ...
        Q20 ~~ Q21         # 残差共分散（モデルにより追加）
```

`lrt_chain` はネストモデル列（M0 = ICM-CFA → 残差共分散を順次追加）で、`lavTestLRT` による robust χ² 差検定に使われます。`<model_name>` 以外のキー（`lrt_chain` など）はモデル走査時に自動的に除外されます。

### 2.13 ESEM 設定・モデル

```yaml
esem_settings:
  estimator: "WLSMV"
  missing: "pairwise"
  se: "robust"
  rotation: "geomin"
  rotation_args:
    geomin.epsilon: 0.001
    rstarts: 30
    algorithm: "gpa"
    std.ov: true
    row.weights: "none"

esem_models:
  <dataset>:
    <model_name>:
      name: "<表示名>"
      n_factors: 5
      factor_names: ["factor_1", ... ]
      display_names: { factor_1: "...", ... }
      items: ["Q01", "Q02", ... ]   # 構文生成とデータ抽出に使用
```

ESEM は全因子を 1 つの EFA ブロック（`efa("efa1")*...`）にまとめ、`rotation_args` を `lavaan` の `rotation.args` にそのまま渡します。

---

## 3. 設定変更のよくある操作

| やりたいこと | 変更箇所 |
|---|---|
| 解析対象データセットを変える | `data_source.dataset` |
| 尺度を 5 件法にする | `global.scale.max: 5` |
| EFA の抽出法・因子数を変える | `efa_settings.extraction_methods` / `n_factors_list` |
| 因子（下位尺度）構成を変える | `scale_structure.<dataset>` |
| 感度分析で項目を除外検討する | 各分析の `sensitivity_exclude_items` |
| CFA モデルを追加する | `cfa_models.<dataset>.<新モデル名>`（必要なら `lrt_chain` にも追加） |
| 妥当性仮説を追加する | `validity_hypotheses.H7` ... |
