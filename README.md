# surveyMeta

社会調査の調査票・報告書PDFから構造化メタデータを自動抽出するRパッケージ。

LLMのコンテキストウィンドウに調査文書と統制語彙表を丸ごと入力し、1回のAPI呼び出しでDDI/JDCat準拠のメタデータをJSON形式で返す。RAGやチャンク分割は使わず、文書全体を一括処理する設計。

2つの抽出機能を提供する：

- **調査レベルメタデータ** (`extract_metadata`): 調査名、概要、標本抽出方法、CESSDAトピック等の16フィールド
- **変数レベルメタデータ** (`extract_variables`): 調査票の全質問項目から変数ラベル・値ラベルを抽出し、コードブックを生成

対応モデル: [Anthropic Claude](https://www.anthropic.com/)（デフォルト）、[Google Gemini](https://ai.google.dev/)（無料枠あり）

## インストール

```r
remotes::install_github("ShoFujihara/surveyMeta")
```

## APIキーの設定

このパッケージはLLM APIを利用する。Rのコードから直接リクエストを送り、構造化された結果を受け取る。

### Claude（デフォルト）

有料。調査1件あたり約$0.20（約30円）。

1. [Anthropic Console](https://console.anthropic.com/) でアカウント作成
2. [API Keys](https://console.anthropic.com/settings/keys) で「Create Key」をクリック
3. `.Renviron` にキーを保存:

```r
usethis::edit_r_environ()
# 以下の1行を追加して保存
# ANTHROPIC_API_KEY=sk-ant-api03-xxxxx
```

4. Rを再起動

### Gemini（無料枠あり）

無料枠あり。1日1,500リクエストまで無料。有料でも調査1件あたり約$0.004（約0.6円）。

1. [Google AI Studio](https://aistudio.google.com/apikey) でAPIキーを取得
2. `.Renviron` にキーを保存:

```r
usethis::edit_r_environ()
# 以下の1行を追加して保存
# GEMINI_API_KEY=AIzaSy...
```

3. Rを再起動

## クイックスタート

### 調査レベルメタデータの抽出

```r
library(surveyMeta)

# Claude（デフォルト）
result <- extract_metadata("questionnaire.pdf")

# Gemini（無料枠で利用可能）
result <- extract_metadata("questionnaire.pdf", model = "gemini-2.0-flash")

result$survey_overview
#> "本調査は、2015年SSM調査研究会が2015年に実施した、日本全国の
#>  20〜79歳の男女を対象とした社会階層と社会移動に関する全国調査であり、
#>  職業・教育・所得等の社会的地位の世代間移動と世代内移動の実態を
#>  明らかにするものである。"

result$sampling_procedure
#> [1] "確率: 層化抽出" "確率: 多段抽出"

result$cessda_topic
#> [1] "社会階層・集団" "平等・不平等・社会的排除"
#> [3] "社会的・職業的移動" "雇用" "教育"

# 根拠の確認（原文引用＋ページ番号）
result$evidence$sampling_procedure
#> "「層化2段無作為抽出法」（p.1）"
```

### 変数レベルメタデータの抽出

調査票PDFから変数ラベル・値ラベルを自動抽出し、コードブックの叩き台を生成する。

```r
vars <- extract_variables("questionnaire.pdf")
vars
#> === Variable-Level Metadata ===
#>   Variables: 85
#>   Value labels: 412
#>
#>   Q1       性別                           [single] 1=男性, 2=女性
#>   Q2       生年月日                       [numeric]
#>   Q3       最終学歴                       [single] 1=中学校, 2=高等学校, 3=短大・高専, ...
#>   Q4       就業状態                       [single] 1=正規雇用, 2=非正規雇用, ...
#>   ...

# Excelコードブックとして出力
export_codebook(vars, "codebook.xlsx")

# CSV/JSON出力も可能
export_codebook(vars, "codebook.csv", format = "csv")
```

## 複数文書の同時入力

調査票と報告書を同時に入力すると、サンプルサイズや調査方法の記述が補完され精度が向上する。

```r
result <- extract_metadata(c(
  "questionnaire.pdf",
  "report.pdf"
))

result$sample_size
#> $planned
#> [1] 15162
#> $valid_responses
#> [1] 7817
#> $response_rate
#> [1] 51.6
```

## PDF読み取りモード

PDF品質に応じて3つのモードを選択できる。

```r
# デフォルト: PDF品質を自動評価し最適な方法を選択
result <- extract_metadata("report.pdf", pdf_mode = "hybrid")

# テキスト抽出のみ（最安）
result <- extract_metadata("report.pdf", pdf_mode = "text")

# スキャンPDFにはVision APIで直接読み取り
result <- extract_metadata("scanned.pdf", pdf_mode = "vision")
```

`hybrid`モードでは、`assess_pdf_quality()` でPDFごとにテキスト抽出品質を0〜1のスコアで評価し、品質が低いPDFのみVisionに切り替える。

## 統制語彙の変換

抽出結果は日本語ラベルで出力される。DDIコードや英語ラベルへの変換が可能。

```r
convert_metadata_labels(result, from = "label_ja", to = "code")$sampling_procedure
#> [1] "Probability.Stratified" "Probability.Multistage"

convert_metadata_labels(result, from = "label_ja", to = "label_en")$cessda_topic
#> [1] "Social stratification and groupings"
#> [2] "Equality, inequality and social exclusion"
#> [3] "Social and occupational mobility" ...
```

## エクスポート

```r
# 調査レベルメタデータ
export_metadata(result, "metadata.csv")
export_metadata(result, "metadata.xlsx", format = "xlsx")
export_metadata(result, "metadata.json", format = "json")

# 変数レベルメタデータ（コードブック）
export_codebook(vars, "codebook.xlsx")
export_codebook(vars, "codebook.csv", format = "csv")
export_codebook(vars, "codebook.json", format = "json")
```

## バッチ処理

```r
surveys <- list(
  c("ssm2015_questionnaire.pdf", "ssm2015_report.pdf"),
  c("jgss2010_questionnaire.pdf"),
  c("nfrj2008_questionnaire.pdf", "nfrj2008_codebook.pdf")
)
batch_result <- batch_extract_metadata(surveys)
export_metadata(batch_result, "all_metadata.csv")
```

## 対応モデル

| モデル | 費用/調査 | 特徴 |
|---|---|---|
| `claude-sonnet-4-5-20250929` (デフォルト) | ~$0.20 | 高精度、prompt caching対応 |
| `gemini-2.0-flash` | ~$0.004 (無料枠あり) | 低コスト、1日1,500件まで無料 |
| `gemini-2.5-pro` | ~$0.05 | Gemini最高精度 |

```r
result <- extract_metadata("report.pdf", model = "gemini-2.0-flash")
result <- extract_metadata("report.pdf", model = "claude-sonnet-4-5-20250929")
```

## コスト確認

```r
cost_summary(result)
#> -- API Cost Summary --
#> Model: claude-sonnet-4-5-20250929
#>
#> Token Usage
#>   Input:       52,180
#>   Output:      3,412
#> Cost (USD)
#>   Total:       $0.2078
#>
#> Processing time: 18.3 seconds
```

## 評価

OAI-PMHでSSJDA公式メタデータを取得し、抽出結果をフィールド別に自動評価できる。

```r
eval_result <- evaluate_extraction(result, survey_id = "1508")
print(eval_result)
#> === Metadata Extraction Evaluation ===
#>
#>   V data_type            1.000  [exact]
#>   V unit_of_analysis     1.000  [exact]
#>   V sampling_procedure   1.000  [set: P=1.00 R=1.00 F1=1.00]
#>   V cessda_topic         0.857  [set: P=1.00 R=0.75 F1=0.86]
#>   V survey_overview      0.812  [text: Jaccard]
#>   ...
```

## 抽出フィールド一覧

調査レベルメタデータは以下の16フィールドを抽出する（JDCatメタデータスキーマ対応）。

| フィールド | JDCat対応 | 内容 | 型 |
|---|---|---|---|
| title | タイトル ◎ | 調査名 | テキスト |
| survey_overview | 概要 ○ | 調査の概要 | テキスト |
| data_type | データタイプ ○ | 量的/質的/官庁統計 | 統制語彙 |
| data_language | データの言語 ○ | ISO 639-3 | 統制語彙 |
| survey_target | 母集団 △ | 母集団の定義 | テキスト |
| unit_of_analysis | 観察単位 ○ | 個人/世帯/組織等 | DDI統制語彙 |
| sample_size | 回収率 | 標本数・有効回答数・回収率 | オブジェクト |
| survey_date | 調査日 | 調査実施期間 | テキスト |
| reference_period | 対象時期 ○ | データ参照時期 | テキスト |
| geographic_coverage | 対象地域 ○ | 地理的範囲 | テキスト |
| sampling_procedure | サンプリング方法 △ | 標本抽出方法 | DDI統制語彙 |
| mode_of_collection | 調査方法 △ | データ収集方法 | DDI統制語彙 |
| survey_conductor | 作成者 ◎ | 調査実施機関 | テキスト |
| main_survey_items | - | 質問項目一覧 | 配列 |
| cessda_topic | トピック ○ | CESSDA Topic v2.0 | 統制語彙 |
| ssjda_topic | - | SSJDAトピック | 統制語彙 |

JDCat対応: ◎必須 ○強く推奨 △推奨

変数レベルメタデータは各質問項目について以下を抽出する。

| フィールド | 内容 |
|---|---|
| question_number | 質問番号（Q1, Q2-1等） |
| label | 変数ラベル（30字程度の簡潔な要約） |
| type | 質問タイプ（single/multiple/numeric/text/scale/ranking/other） |
| code | 値コード |
| value_label | 値ラベル |
| condition | 回答条件（スキップパターン） |
| page | ページ番号 |

## 引用

```r
citation("surveyMeta")
```

> Fujihara, S. (2026). surveyMeta: Extract Structured Metadata from Social Survey Documents Using LLM APIs. https://github.com/ShoFujihara/surveyMeta

## License

MIT
