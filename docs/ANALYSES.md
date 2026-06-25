# 分析モジュール API リファレンス

各分析の公開関数・引数・統計手法・参照設定・出力・評価しきい値をまとめます。すべての公開関数は `keyed_data` オブジェクト（`prepare_data()` の返り値要素、例: `data$target`）を受け取ります。

> 凡例: 評価しきい値の所在を **[YAML]**（設定で変更可）/ **[コード]**（ハードコード）で示します。

---

## 0. データ準備

### `prepare_data()`
- **引数**: なし
- **処理**: 設定読込 → Google Sheets 認証 → target / rehab / interrater を取得し型変換・キー付与
- **返り値**: `list(target, rehab, interrater, config)`（各データは `keyed_data`）

詳細は [ARCHITECTURE.md](ARCHITECTURE.md) を参照。

---

## 1. 回答品質スクリーニング

### 1.1 Satisficing（手抜き回答の検出）

依存: `careless`

| 関数 | シグネチャ |
|---|---|
| `analyze_satisficing(data)` | `data` は `prepare_data()` の返り値全体（内部で `data$target` のみ使用） |
| `create_clean_dataset(data_obj, satisficing_results, dataset = "target")` | flag された回答者を除外した `keyed_data` を返す |
| `get_flagged_ids(satisficing_results, dataset = "target")` | flag されたインデックス・ID を返す |

**検出指標（OR 結合 = いずれか該当で flag）:**

| 指標 | 計算 | flag 条件 | しきい値 |
|---|---|---|---|
| Longstring (LS) | `careless::longstring` 連続同一回答の最大長 | `LS > threshold` | 10 [YAML] |
| IRV | `careless::irv` 個人内回答変動 | `IRV < quantile(percentile)`（下位 5%） | 5 [YAML] |
| Mahalanobis D² | `careless::mahad` → `pchisq(D², df=項目数)` | `p < p_threshold` | 0.001 [YAML] |

`create_clean_dataset()` は `any_flag` が立たない行のみ残し、新しい `keyed_data` を返します。

### 1.2 Response Styles（回答スタイル）

| 関数 | シグネチャ |
|---|---|
| `analyze_response_styles(data)` | 内部で `data$target` のみ使用 |
| `create_rs_clean_dataset(data_obj, rs_results, dataset = "target", exclude_type = "ers")` | `exclude_type` ∈ `{"ers","mrs","both"}` |
| `get_rs_flagged_ids(rs_results, dataset = "target", type = "ers")` | `type` ∈ `{"ers","mrs"}` |

**指標:**

| 指標 | 計算 | flag 条件 |
|---|---|---|
| ERS（極端反応） | 最小・最大カテゴリ回答の割合 = `n_extreme / n_valid` | `ERS > quantile(95)` [YAML] |
| MRS（中間反応） | 中間カテゴリ（例: 1–4 なら 2,3）回答の割合 | `MRS > quantile(95)` [YAML] |

加えて ERS/MRS と合計得点の相関（`cor.test`）も算出。**ARS（黙従反応）は未実装。**

---

## 2. 項目分析

### 2.1 GP 分析（Good-Poor / 識別力）

| 関数 | シグネチャ |
|---|---|
| `analyze_gp(data_obj)` | 全パイプライン（群分け → 識別力 → CSV 出力） |
| `show_gp_evaluation(data_obj)` | 評価判定のみ表示 |

- **群分け**: 合計得点の上位/下位 27%（`quantile`）。`gp_analysis.cutoff_percentile` [YAML]
- **識別力指標**:
  - D\* = `(mean_good − mean_poor) / (尺度幅)`
  - Cohen's d, Hedges' g（CI 付き）via `psych::cohen.d`
- **参照設定**: `gp_analysis.cutoff_percentile`, `global.scale`
- **出力**: `output/gp_analysis_results.csv`
- **評価しきい値** [コード]:
  - D\*: ≥0.40 優 / ≥0.30 良 / ≥0.20 可 / <0.20 不良（flag）
  - d・g: ≥0.80 大 / ≥0.50 中 / ≥0.20 小 / <0.20 無視（flag）
  - 総合: 問題項目 0=OK、≤3=要確認、>3=識別力不十分

### 2.2 項目間相関（Inter-Item）

| 関数 | シグネチャ |
|---|---|
| `analyze_item_correlations(data_obj)` | polychoric / Pearson 相関行列・MIIC を表示 |
| `show_ii_evaluation(data_obj)` | 分布・下位尺度・クラスタの評価 |

- **相関**: polychoric（`psych::polychoric`）と Pearson（`cor`, `correlation_analysis.use` [YAML]）
- **MIIC（平均項目間相関）**: `psych::cor.smooth` → `psych::alpha` の `average_r`（両相関で算出）
- **クラスタ検出**: `r > 0.70` の連結成分（冗長項目候補）[コード]
- **強度区分** [コード]: 極高 >0.70 / 高 0.50–0.70 / 中 0.30–0.50 / 低 <0.30
- **参照設定**: `correlation_analysis.use`, `scale_structure[[dataset]]`
- > ⚠ **既知バグ**: `show_ii_evaluation()` の下位尺度評価が `calculate_subscale_correlations()` に必須引数 `n_obs` を渡しておらず、この経路でエラーになります。

### 2.3 項目-合計相関（Item-Total）

| 関数 | シグネチャ |
|---|---|
| `analyze_item_total(data_obj)` | 全体＋下位尺度の I-T 相関、3 つの CSV 出力 |
| `show_it_evaluation(data_obj)` | 修正済み I-T の段階評価 |

- **手法**: Pearson（`psych::alpha` の `r.drop`=修正済み, `raw.r`=非修正）＋ polyserial（`polycor::polyserial`）の 4 系列
- **参照設定**: `scale_structure[[dataset]]`（下位尺度が定義されていれば自動有効）
- **出力**: `output/it_correlations.csv`, `it_subscale_correlations.csv`, `it_comparison.csv`
- **評価しきい値**（修正済み I-T）[コード]: <0.20 不良 / 0.20–0.30 限界 / 0.30–0.40 良 / ≥0.40 優

### 2.4 天井・床効果（Ceiling/Floor）

| 関数 | シグネチャ |
|---|---|
| `analyze_ceiling_floor(data_obj)` | 記述統計＋床/天井% を表示、CSV 出力 |
| `show_cf_evaluation(data_obj)` | しきい値ベースの flag 評価 |

- **計算**: 項目ごとに mean / sd / 歪度（`psych::skew`）/ 尖度（`psych::kurtosi`）、床% = `==min` 割合、天井% = `==max` 割合
- **参照設定**: `global.scale.min` / `max`
- **出力**: `output/ceiling_floor_results.csv`, `ceiling_floor_evaluation.csv`
- **評価しきい値** [コード]: 床/天井 30/25/20/15% の段階 flag、|歪度|>1.0、|尖度|>2.0、Mean±SD が尺度範囲外

---

## 3. 信頼性

| 関数 | シグネチャ |
|---|---|
| `analyze_reliability(data_obj)` | α と ω を全体＋下位尺度で算出・表示 |
| `calculate_cronbach_alpha(data, subscale_defs = NULL)` | calculator |
| `calculate_omega(data, nfactors, subscale_defs = NULL)` | calculator（`nfactors` 必須） |

- **Cronbach's α**: `psych::alpha(check.keys=FALSE)`。raw / std α、項目除外時 α（`delta_alpha` 付き）
- **McDonald's ω**: `psych::omega(fm="minres", flip=FALSE)`。`omega_total`、`omega_h`（全体のみ、単因子では NA）
- **単位**: 全体尺度（全項目、`nfactors`=下位尺度数の双因子モデル）＋各下位尺度（α: 項目≥2、ω: 項目≥3）
- **参照設定**: `data_source.dataset`, `scale_structure[[dataset]]`
- **評価しきい値**: なし（数値のみ表示、自動判定なし）

---

## 4. 因子分析

### 4.1 因子分析適合性（Suitability）

| 関数 | シグネチャ |
|---|---|
| `check_fa_suitability(data_obj)` | KMO・Bartlett・特異性・ゼロセル診断を実行 |
| `show_fa_evaluation(results)` | 評価判定を表示 |

- **KMO**: `psych::KMO`（polychoric / Pearson 両方）
- **Bartlett 球面性検定**: `psych::cortest.bartlett`（Pearson 行列）
- **特異性診断**: 固有値・ランク（`Matrix::rankMatrix`）・条件数・rcond・Cholesky 可否
- **ゼロセル診断**: polychoric 推定を不安定にする疎なクロス表の検出
- **参照設定**: `global.item_pattern`, `global.scale`
- **評価しきい値** [コード]: N:p ≥5（理想≥10）、Bartlett p<.05、KMO/MSA ≥0.50（0.60=境界）。Kaiser の「miserable〜marvelous」段階

### 4.2 因子数の決定

依存: `psych`, `EFA.MRFA`

| 関数 | シグネチャ |
|---|---|
| `determine_factors(data_obj, n_iterations = NULL, percentile = NULL, seed = NULL, verbose = TRUE)` | 因子数を判定（NULL は設定値） |
| `show_fn_evaluation(data_obj, ...)` | 手法間一致を評価表示（再計算する） |

- **手法**:
  - 平行分析 MRFA（`EFA.MRFA::parallelMRFA`）
  - 平行分析 FA / PCA（`psych::fa.parallel`, `fm="minres"`）
  - MAP（Velicer, `psych::VSS$map`）
  - Kaiser 基準（PCA 固有値 >1）
  - polychoric / Pearson 双方で実行
- **参照設定**: `factor_number.parallel_analysis_iterations`(1000), `.percentile`(95), `.sensitivity_exclude_items`
- **感度分析**: `generate_exclude_subsets()` で除外項目の冪集合を生成し各サブセットで再実行
- **出力**: 各手法の推奨因子数と一致度（モード・範囲）。数値の合否判定はなし

### 4.3 探索的因子分析（EFA）

依存: `psych`, `GPArotation`, `clue`

| 関数 | シグネチャ |
|---|---|
| `analyze_efa(data_obj, verbose = TRUE, show_full_results = TRUE)` | グリッド全探索＋CSV 出力 |
| `show_efa_evaluation(data_obj)` | 各解の合否（適格セット）を表示・出力 |

- **抽出**: `psych::fa(rotate="none", fm=...)`（ml では SMC 無効化）
- **回転**: oblimin（`GPArotation::oblimin`, gamma ループ）と promax（`stats::promax`, kappa ループ）
- **グリッド**: サブセット × `n_factors_list` × `extraction_methods` × {polychoric, Pearson} × {gamma, kappa}
- **整列**: Pearson 解を polychoric 解に整列（`clue::solve_LSAP` × `psych::factor.congruence`）
- **参照設定**: `efa_settings.*`（missing, pd_tolerance, extraction_methods, n_factors_list, gamma_values, kaiser_normalize, max_iterations, flip_factors, promax_kappa_values, display_cutoff, sensitivity_exclude_items）, `efa_evaluation.*`
- **出力**: `efa_output/pattern_<...>.csv`（パターン行列）
- **項目評価** [YAML]: 主負荷 ≥ `primary_threshold`(0.32)、交差負荷 = 二次 ≥ `cross_threshold`(0.2) または 主-二次差 ≤ `diff_threshold`(0.1)。0 失敗の解を「適格セット」として全結果（パターン・構造・因子相関・共通性・分散説明率）を表示

### 4.4 確認的因子分析（CFA）

依存: `lavaan`

| 関数 | シグネチャ |
|---|---|
| `analyze_cfa(data_obj, model_name, display_results = TRUE)` | 単一モデルの CFA |
| `compare_cfa_models(data_obj, display_individual = FALSE)` | 全モデル比較＋尤度比検定 |
| `extract_cfa_results(results, what = "all")` | 結果の抽出（`what` ∈ all/fit/residuals/mi/reliability/parameters/problems） |
| `show_cfa_summary(results)` | 要約表 |

- **推定**: `lavaan::cfa(estimator="WLSMV", missing="pairwise", se="robust", ordered=...)`（順序カテゴリカル、marker 変数法）
- **適合度**: χ²（scaled）, CFI/TLI（robust）, RMSEA（CI・pclose）, SRMR, AIC/BIC ほか（`fitMeasures`）
- **診断**: 残差（`lavResiduals`）、修正指標（`modificationIndices`）、推定問題（収束・Heywood・条件数）
- **信頼性/妥当性**: CR・AVE・MSV・ASV、Fornell-Larcker（√AVE）
- **尤度比検定**: `lavTestLRT`（robust χ² 差検定）を `lrt_chain` のネスト列に適用
- **参照設定**: `cfa_settings.*`, `cfa_models[[dataset]]`（model_syntax, display_names, lrt_chain）

### 4.5 ESEM

依存: `lavaan`

| 関数 | シグネチャ |
|---|---|
| `analyze_esem(data_obj, model_name, display_results = TRUE)` | 単一モデルの ESEM |
| `compare_cfa_esem(data_obj, model_name, display_individual = FALSE)` | CFA と ESEM を同名モデルで比較 |
| `show_esem_summary(results)` | 要約表 |

- **構文生成**: 全因子を 1 つの EFA ブロックに（`efa("efa1")*factor_1 + ... =~ Q01 + ...`）
- **推定**: `lavaan::sem(estimator="WLSMV", rotation="geomin", rotation.args=...)`
- **負荷量**: 各項目を最大絶対負荷の因子に割り当て、CR/AVE を算出
- **参照設定**: `esem_settings.*`, `esem_models[[dataset]]`（n_factors, factor_names, items, display_names）
- **CFA vs ESEM 比較**: 適合度の併記、ESEM 負荷行列、因子相関・CR/AVE の比較（共通因子名で突合）

> `compare_cfa_esem()` は対象データセットに同名モデルが `cfa_models` と `esem_models` の双方に存在する必要があります。

---

## 5. 妥当性（Validity）

| 関数 | シグネチャ |
|---|---|
| `analyze_validity(target_obj, rehab_obj)` | REHAB を基準とした収束・弁別妥当性 |

- **得点**: 下位尺度・合計とも素点の行合計（`rowSums`）。target 因子は `scale_structure`、REHAB は `rehab_subscales`
- **仮説検証**（H1〜H6, `validity_hypotheses` [YAML]）:
  - target-proximal と target-distal の相関を比較
  - `abs_diff = |r(target,proximal)| − |r(target,distal)|`
  - Steiger（1980）従属相関差検定（`psych::r.test`）、片側 p
  - `supported = (abs_diff > 0) かつ (p_one_sided < 0.05)`
- **相関法**: 主分析 = Pearson、感度分析 = Spearman（polychoric は不使用）
- **補助統計**: 相関の CI（Pearson は `cor.test`、Spearman は Fisher z）と検出力（`pwr::pwr.r.test`）
- **解釈の前提**: REHAB は高値=機能低下のため、負の相関が収束妥当性を示す
- **評価**: Steiger 検定の `supported`（Yes/No）。Landis-Koch 等の段階ラベルはなし

---

## 6. 評価者間信頼性（Inter-rater）

依存: `irr`, `irrCAC`, `psych`, `boot`, `BlandAltmanLeh`

| 関数 | シグネチャ |
|---|---|
| `analyze_interrater(data_obj)` | 項目・尺度レベルの一致係数（感度分析対応） |

- **ペア構成**: 患者 ID で 2 行ちょうどの患者のみ採用。行順で rater1 / rater2 を定義
- **項目レベル**:
  - 一致率 `p_o`（`irr::agree`）
  - 重み付き κ（`irr::kappa2(weight="squared")`、ブートストラップ 95%CI）
  - Gwet's AC2（`irrCAC::gwet.ac1.raw`, quadratic 重み）
- **尺度レベル**:
  - ICC(2,1)・ICC(2,2)（`psych::ICC`）
  - 合計得点の AC2/AC1
  - Bland-Altman（`BlandAltmanLeh::bland.altman.stats`）
- **参照設定**: `interrater_analysis.bootstrap_iterations`(1000), `.ac2_weights`(quadratic), `.sensitivity_exclude_items`(["Q19"]), `data_keys.interrater`, `global.item_pattern`, `global.scale`
- **感度分析**: `generate_exclude_subsets()`（full / excl_Q19）。各サブセットで全係数を再計算
- **評価**: 要約で κ<0.5・AC2<0.5 の項目数を計上（0.5 が唯一の境界）。Landis-Koch 段階表は非表示
- > 注: `kappa_weight` 設定は読み込まれるが、`kappa2` には文字列 `"squared"` がハードコードされており実質的に未使用。尺度レベルの重み付き κ は計算されない（ICC・AC2・Bland-Altman を表示）

---

## 7. 可視化（Visualization）

依存: `ggplot2`, `gridExtra`, `scales`

| 関数 | シグネチャ |
|---|---|
| `visualize_data(data_obj)` | 3 種の図を描画し、計算結果を invisible で返す |

- **図**:
  1. 項目別回答分布（カテゴリ別棒グラフのグリッド）
  2. 合計得点分布（正規曲線オーバーレイ付きヒストグラム）
  3. 項目間相関ヒートマップ（r 値表示）
- **参照設定**: `global.scale.min` / `max`, `correlation_analysis.use`
- **出力**: ファイル保存はせず、アクティブなグラフィックスデバイスへ `print()`

---

## 評価しきい値の所在（早見表）

| 分析 | しきい値 | 所在 |
|---|---|---|
| GP | D\*・d・g の段階、群分け 27% | コード（27%のみ YAML） |
| 項目間相関 | クラスタ r>0.70、強度区分 | コード |
| 項目-合計 | <0.20/0.30/0.40 | コード |
| 天井・床 | 30/25/20/15%、\|歪度\|>1、\|尖度\|>2 | コード |
| 信頼性 | （なし） | — |
| FA 適合性 | N:p≥5、Bartlett p<.05、KMO≥0.50 | コード |
| EFA 項目評価 | primary 0.32 / cross 0.2 / diff 0.1 | YAML |
| 妥当性 | Steiger 片側 p<.05 | コード |
| 評価者間 | κ/AC2 <0.5 | コード |
