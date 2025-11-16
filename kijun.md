# 尺度開発における統計的判断基準チェックリスト

## 概要
このドキュメントは、心理尺度開発における各種統計分析の判断基準をまとめたチェックリストです。
機械的な適用ではなく、理論的妥当性と併せて総合的に判断することが重要です。

## 目次
1. [項目分析](#1-項目分析)
2. [信頼性分析](#2-信頼性分析)
3. [因子分析](#3-因子分析)
4. [妥当性](#4-妥当性)
5. [評価者間信頼性](#5-評価者間信頼性)
6. [総合的な判断の流れ](#6-総合的な判断の流れ)
7. [報告すべき統計量](#7-報告すべき統計量)

---

## 1. 項目分析

### 1.1 GP分析（Good-Poor Analysis）

#### 判断基準
- **D* (Discrimination Index)**
  - `D* > 0.30` → 良好な弁別力
  - `D* = 0.20-0.30` → 許容範囲
  - `D* < 0.20` → 要検討（削除候補）

- **効果量**
  - `Cohen's d > 0.80` → 大きな効果
  - `Cohen's d = 0.50-0.80` → 中程度の効果
  - `Cohen's d < 0.50` → 小さな効果

#### 注意点
- サンプルサイズが小さい場合は効果量を重視
- 理論的重要性の高い項目は基準が低くても保持を検討

### 1.2 天井・床効果（Ceiling-Floor Effects）

#### 判断基準
- **出現率**
  - 天井効果 `< 15%` → 良好
  - 床効果 `< 15%` → 良好
  - いずれか `> 30%` → 要修正または削除

- **分布の形状**
  - `|Skewness| < 2` → 許容範囲
  - `|Kurtosis| < 7` → 許容範囲
  - 平均±SD が尺度範囲内 → 良好

#### 対処法
- 天井効果 → より難しい項目への変更
- 床効果 → より易しい項目への変更
- 両極端の選択肢追加を検討

### 1.3 項目間相関（Item-Item Correlation）

#### 判断基準
- **MIIC (Mean Inter-Item Correlation)**
  - `MIIC = 0.15-0.50` → 適切な範囲
  - `r > 0.70` → 冗長性あり（どちらか削除検討）
  - `r < 0.10` → 関連性低い（因子の一貫性に問題）

- **相関の種類**
  - Polychoric vs Pearson の差が大きい（`> 0.15`）→ 順序尺度の特性が強い
  - 4件法以下の場合はPolychoric推奨

### 1.4 項目合計相関（Item-Total Correlation）

#### 判断基準
- **Corrected Item-Total Correlation**
  - `r > 0.30` → 良好
  - `r = 0.20-0.30` → 許容範囲
  - `r < 0.20` → 削除候補
  - `r > 0.80` → 冗長性の疑い

- **相関の種類**
  - Polyserial: 理論的に適切（順序×連続）
  - Pearson: 実用上の差が小さければ可

---

## 2. 信頼性分析

### 2.1 Cronbach's Alpha

#### 判断基準
- `α > 0.90` → 優秀（ただし冗長性に注意）
- `α = 0.80-0.90` → 良好
- `α = 0.70-0.80` → 許容範囲
- `α = 0.60-0.70` → 要改善（研究初期なら許容）
- `α < 0.60` → 問題あり

#### 用途別基準
| 用途 | 必要な信頼性 |
|------|------------|
| 個人診断用 | α > 0.90 |
| 集団比較研究 | α > 0.70 |
| 探索的研究 | α > 0.60 |

#### 項目削除による改善
- α増加 `> 0.05` → 削除を強く検討
- α増加 `= 0.01-0.05` → 理論的重要性と併せて判断

### 2.2 McDonald's Omega

#### 判断基準
- `ω_total > α` → 因子構造の存在を示唆
- `ω_hierarchical > 0.70` → 一般因子の寄与大
- `ω_h / ω_t > 0.75` → 一次元性高い
- `ω_h / ω_t < 0.50` → 多次元性の疑い

#### 解釈のポイント
- **ω_total**: 全体の信頼性
- **ω_h**: 一般因子による信頼性
- 差が大きい → 下位因子の独自性が高い

---

## 3. 因子分析

### 3.1 因子分析の前提条件

#### KMO (Kaiser-Meyer-Olkin)
| KMO値 | 判定 |
|-------|------|
| > 0.90 | 素晴らしい |
| 0.80-0.90 | 良好 |
| 0.70-0.80 | まあまあ |
| 0.60-0.70 | 平凡 |
| < 0.60 | 不適切 |

#### その他の前提
- Bartlett's test: `p < 0.05` → 因子分析可能
- サンプルサイズ比: `N/p > 10:1` （理想）、`N/p > 5:1` （最低限）

### 3.2 EFA - 因子数決定

#### 判断基準の優先順位
1. **理論的根拠**（最優先）
2. **Parallel Analysis**（統計的に最も推奨）
3. **MAP test**
4. **Kaiser基準**（参考程度）

#### 因子数の妥当性
- PA推奨数 = MAP推奨数 → 強い根拠
- 推奨が分かれる → 複数解を検討
- スクリープロット → 「肘」の確認（補助的）

### 3.3 EFA - 因子構造の評価

#### 因子負荷量
| 負荷量 | 判定 |
|--------|------|
| λ > 0.70 | 優秀 |
| λ = 0.50-0.70 | 良好 |
| λ = 0.40-0.50 | 許容範囲 |
| λ = 0.30-0.40 | 境界線 |
| λ < 0.30 | 削除検討 |

#### クロス負荷
- 主負荷 - 副負荷 `> 0.20` → 単純構造
- 複数負荷 `> 0.30` → 項目の再検討

#### 因子相関
- `r < 0.85` → 因子の弁別性あり
- `r > 0.85` → 因子の統合を検討
- `r < 0.30` → 直交回転を検討

#### 共通性
- `h² > 0.40` → 良好
- `h² = 0.20-0.40` → 許容範囲
- `h² < 0.20` → 因子での説明不十分

### 3.4 CFA - モデル適合度

#### 絶対適合度指標
| 指標 | 良好 | 許容範囲 | 不適合 |
|------|------|---------|--------|
| χ²/df | < 2 | < 3 | > 5 |
| RMSEA | < 0.05 | < 0.08 | > 0.10 |
| SRMR | < 0.05 | < 0.08 | > 0.10 |

#### 増分適合度指標
| 指標 | 良好 | 許容範囲 | 不適合 |
|------|------|---------|--------|
| CFI | > 0.95 | > 0.90 | < 0.90 |
| TLI | > 0.95 | > 0.90 | < 0.90 |

#### 情報量基準（モデル比較用）
- **AIC/BIC**: 小さいほど良い
- **ΔCFI > 0.01**: 実質的な改善

### 3.5 CFA - パラメータ評価

#### 因子負荷量
- 標準化λ `> 0.50` → 良好
- 標準化λ `< 0.30` → 問題あり
- すべて統計的有意 → 必須

#### 因子相関
- `r < 0.85` → 弁別妥当性あり
- `r > 0.90` → 統合を検討

#### R²（項目の説明率）
- `R² > 0.50` → 良好
- `R² = 0.30-0.50` → 許容範囲
- `R² < 0.30` → 要検討

#### 残差
- 標準化残差 `< |2|` → 良好
- 標準化残差 `> |4|` → 問題あり

### 3.6 CFA判定フロー（Likert→WLSMV/DWLS前提）の数理

#### 3.6.1 初期モデルの適合（全体適合＋局所診断）

**モデル**
  
$$
\Sigma(\theta)=\Lambda\Phi\Lambda'+\Theta,
$$

\(\Lambda\)：因子負荷，\(\Phi\)：因子共分散，\(\Theta\)：誤差共分散。

**不一致関数（DWLS/WLS）**  
サンプル統計（相関・閾値など）ベクトル \(s\)、モデル期待 \(\sigma(\theta)\)、重み行列 \(W\)：

$$
F_{\mathrm{WLS}}(\theta)=\{s-\sigma(\theta)\}'\,W\,\{s-\sigma(\theta)\}.
$$

**WLSMVのスケール補正つき検定統計**

$$
T_{\mathrm{MV}}=(N-1)\,c\,F_{\mathrm{WLS}}(\hat{\theta})\ \dot{\sim}\ \chi^2_{df},
$$

\(c\)：平均・分散補正係数。

**ロバスト適合指標**

$$
\mathrm{CFI}_r
=1-\frac{\max(T_r-df,0)}{\max(T^{\mathrm{null}}_r-df^{\mathrm{null}},0)},
$$

$$
\mathrm{TLI}_r
=\frac{T^{\mathrm{null}}_r/df^{\mathrm{null}}-T_r/df}{T^{\mathrm{null}}_r/df^{\mathrm{null}}-1},
$$

$$
\mathrm{RMSEA}_r=\sqrt{\frac{\max(T_r-df,0)}{df\,(N-1)}}.
$$

SRMR（相関基準）：

$$
\mathrm{SRMR}=\sqrt{\frac{1}{q}\sum_{k=1}^{q} r_k^2},\quad
r=W^{1/2}\{s-\sigma(\hat{\theta})\}.
$$

**局所診断**  
標準化残差（各統計のz）：

$$
z_k=\frac{s_k-\sigma_k(\hat{\theta})}{\mathrm{SE}(s_k-\sigma_k)}\ \dot{\sim}\ \mathcal{N}(0,1).
$$

修正指標（MI：LM検定）と期待パラメータ変化量（EPC）：

$$
\mathrm{MI}
= g_\psi'\,\mathcal{I}_{\psi\psi}^{-1}\,g_\psi\ \dot{\sim}\ \chi^2_{(1)},\qquad
\mathrm{EPC}_\psi=-\,\mathcal{I}_{\psi\psi}^{-1}\,g_\psi,
$$

\(g\)：スコア，\(\mathcal{I}\)：期待情報行列。実務上，クロス負荷の標準化EPCが \(\gtrsim0.20{-}0.30\) なら実質的。

#### 3.6.2 因子独立（\(\Phi\) 対角）の入れ子検定：DIFFTEST

**仮説**  
\(H_0:\ \phi_{fg}=0\ (f\neq g)\)（独立） vs \(H_1:\ \phi_{fg}\) 自由（斜交）。

**WLSMV差分検定（DIFFTEST）**  
制約付き \(M_0\) と制約なし \(M_1\) から調整済み差分統計 \(T_D\) を構成し，

$$
T_D\ \dot{\sim}\ \chi^2_{(df_0-df_1)}.
$$

（実装はヤコビアン \(A\) とサンプル統計の漸近共分散 \(\Gamma\) を用いるWald型。）  
**判定**：\(p\ge .05\)（かつ \(\Delta\mathrm{CFI}\le .010\)）→独立で十分。\(p<.05\)→独立は過度の制約。

#### 3.6.3 クロス負荷・誤差共分散の逐次解放：DIFFTEST＋MI/EPC

**仮説**  
(1) 非標的負荷 \(\lambda_{i,j\neq t(i)}=0\)（ゼロ固定）  
(2) 誤差共分散 \(\theta_{ij}=0\)（対角）

**手順**  
1) 当該パラメータの **MI と EPC** を確認。  
2) **1つずつ**自由化した \(M_1\) を当て，基準 \(M_0\) と **DIFFTEST**。  
3) 有意で理論根拠が明確なら採用。多数必要なら個別\(\theta_{ij}\)の乱発ではなく**方法因子／testlet因子**を導入。

**因子相関“過大化”の簡約式（参考）**  
2因子・2項目の最小例で真のクロス負荷 \(\delta\) を0固定すると，

$$
\rho^\*\approx \rho+\frac{\delta_{j1}}{\lambda_{j2}}+\frac{\delta_{i2}}{\lambda_{i1}}\quad(\text{一次近似}),
$$

すなわち推定相関 \(\rho^\*\) が押し上がる。経験的サイン：\(\rho^\*\gtrsim .85{-}.90\)。

#### 3.6.4 実務アルゴリズム（Likert→WLSMV/DWLS）

1) 初期CFA **[クロス負荷0｜斜交｜誤差対角]** → \(T_r\), CFI\(_r\), TLI\(_r\), RMSEA\(_r\), SRMR と **z残差／MI／EPC** を確認。  
2) **独立仮説**は **DIFFTEST**（補助に \(\Delta\)CFI \(\le .010\)）。  
3) **クロス負荷／誤差共分散**は **理論根拠**がある最小限のみ**逐次1つずつ**解放→都度 **DIFFTEST**。  
4) \(\rho\) の高騰や \(\theta_{ij}\) 多発なら **ESEM/BSEM／方法因子・testlet**，または因子数・階層（2次／バイファクター）を再検討。

---

## 4. 妥当性

### 4.1 収束妥当性

#### 判断基準
- 同概念尺度との相関 `r > 0.50` → 良好
- `r = 0.30-0.50` → 中程度
- `r < 0.30` → 要検討

#### 注意点
- 既存尺度の信頼性も考慮
- Attenuation correctionの検討

### 4.2 弁別妥当性

#### 判断基準
- 異概念尺度との相関 `|r| < 0.30` → 良好
- `|r| = 0.30-0.50` → 要注意
- `|r| > 0.50` → 弁別性に問題

#### 多特性多方法（MTMM）
- 収束相関 > 弁別相関 → 必須
- 同方法異特性 < 異方法同特性 → 理想

---

## 5. 評価者間信頼性

### 5.1 ICC（級内相関係数）

#### 判断基準（Cicchetti, 1994）
| ICC値 | 判定 |
|-------|------|
| < 0.40 | Poor |
| 0.40-0.59 | Fair |
| 0.60-0.74 | Good |
| > 0.75 | Excellent |

#### 用途別
- **ICC(2,1)**: 単一評価者使用時
- **ICC(2,2)**: 複数評価者平均使用時

### 5.2 重み付きKappa

#### 判断基準（Landis & Koch, 1977）
| κ値 | 判定 |
|-----|------|
| < 0.20 | Slight |
| 0.21-0.40 | Fair |
| 0.41-0.60 | Moderate |
| 0.61-0.80 | Substantial |
| > 0.81 | Almost perfect |

#### 注意点
- 有病率パラドックスに注意
- Gwet's AC2も併用検討

---

## 6. 総合的な判断の流れ

### 6.1 項目選定の優先順位

1. **理論的重要性**（内容妥当性）
2. **因子負荷量**
3. **項目合計相関**
4. **GP分析の弁別力**
5. **天井・床効果**
6. **信頼性への寄与**

#### 削除時の注意
- 一度に複数項目を削除しない
- 削除のたびに再分析
- 最終的に最低3項目/因子は確保

### 6.2 モデル選択の優先順位

1. **理論的妥当性**
2. **解釈可能性**
3. **統計的適合度**
4. **簡潔性**（パーシモニー）

#### トレードオフの考慮
- 適合度 vs 簡潔性
- 統計 vs 理論
- 探索的発見 vs 確認的検証

---

## 7. 報告すべき統計量

### 7.1 必須項目
- [ ] サンプルサイズ、欠測処理
- [ ] 項目の平均、SD、歪度、尖度
- [ ] 信頼性（α、ω）
- [ ] 因子構造（EFA and/or CFA）
- [ ] 適合度指標（CFA実施時）
- [ ] 妥当性の根拠

### 7.2 推奨項目
- [ ] 項目間相関マトリックス
- [ ] 項目合計相関
- [ ] test-retest信頼性（可能なら）
- [ ] 評価者間信頼性（該当時）
- [ ] 既知グループ妥当性
- [ ] 測定不変性（多群比較時）

### 7.3 追補：CFA（Likert／WLSMV）で追加報告すべき事項
- [ ] データの**順序扱い**（`ordered=`）と推定量（**WLSMV/DWLS**），相関種（**polychoric**）  
- [ ] パラメタリゼーション（`theta`/`delta`）と識別（因子尺度固定，閾値の扱い）  
- [ ] **robust**版の **CFI/TLI/RMSEA**，SRMR，\(\chi^2_{scaled}\) と **df**  
- [ ] **DIFFTEST** の結果（独立仮説・逐次解放の各比較），\(\Delta\)CFI  
- [ ] **MI/EPC** の上位候補と**理論根拠**（採用/不採用の理由）  
- [ ] 方法因子／testlet導入の有無と理由（個別\(\theta_{ij}\)の乱発回避）

---

## 注意事項

これらの基準は**目安**であり、以下の点を考慮して柔軟に判断することが重要です：

- 研究文脈
- サンプル特性
- 尺度の用途
- 理論的妥当性と実用性のバランス

**統計的基準のみでなく、理論的妥当性を最優先に考慮してください。**

---

## 参考文献

- Cicchetti, D. V. (1994). Guidelines, criteria, and rules of thumb for evaluating normed and standardized assessment instruments in psychology. *Psychological Assessment*, 6(4), 284–290.  
- Landis, J. R., & Koch, G. G. (1977). The measurement of observer agreement for categorical data. *Biometrics*, 33(1), 159–174.

### 参考文献（追補）
- Asparouhov, T., & Muthén, B. (2006). Robust chi-square difference testing with mean and variance adjusted test statistics. *Mplus Web Notes*, No. 10.  
- Asparouhov, T., & Muthén, B. (2009). Exploratory structural equation modeling. *Structural Equation Modeling*, 16, 397–438.  
- Browne, M. W. (1984). Asymptotically distribution-free methods for analysis of covariance structures. *British Journal of Mathematical and Statistical Psychology*, 37, 62–83.  
- Brown, T. A. (2015). *Confirmatory Factor Analysis for Applied Research* (2nd ed.). Guilford.  
- Cheung, G. W., & Rensvold, R. B. (2002). Evaluating goodness-of-fit indexes for testing measurement invariance. *Structural Equation Modeling*, 9, 233–255.  
- Flora, D. B., & Curran, P. J. (2004). An empirical evaluation of alternative estimation methods for CFA with ordinal data. *Psychological Methods*, 9, 466–491.  
- Hu, L., & Bentler, P. M. (1999). Cutoff criteria for fit indexes in covariance structure analysis. *Structural Equation Modeling*, 6, 1–55.  
- Kline, R. B. (2023). *Principles and Practice of Structural Equation Modeling* (5th ed.). Guilford.  
- Lloret-Segura, S., Ferreres-Traver, A., Hernández-Baeza, A., & Tomás-Marco, I. (2014). Guidelines for EFA. *Anales de Psicología*, 30, 1151–1169.  
- Marsh, H. W., & Hocevar, D. (1988). A new, more powerful approach to MTMM analyses: Correlated uniquenesses. *Journal of Personality*, 56, 638–645.  
- Marsh, H. W., Morin, A. J. S., Parker, P. D., & Kaur, G. (2014). Exploratory Structural Equation Modeling… *Psychological Methods*, 19, 142–165.  
- Muthén, B. (1984). A general SEM with dichotomous, ordered categorical variables. *Psychometrika*, 49, 115–132.  
- Podsakoff, P. M., MacKenzie, S. B., Lee, J.-Y., & Podsakoff, N. P. (2003). Common method biases in behavioral research. *Journal of Applied Psychology*, 88, 879–903.  
- Reise, S. P. (2012). The rediscovery of bifactor models. *Multivariate Behavioral Research*, 47, 667–696.

---

最終更新: 2025年

