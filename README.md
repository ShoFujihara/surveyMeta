# surveyMeta

社会調査の調査票・報告書PDFから、DDI準拠の構造化メタデータをLLMで抽出するRパッケージです。LLMのコンテキストウィンドウに調査文書と統制語彙表を丸ごと入力し、1回のAPI呼び出しで16フィールドのメタデータをJSON形式で返します。

対応モデル: [Anthropic Claude](https://www.anthropic.com/)（デフォルト）、[Google Gemini](https://ai.google.dev/)（無料枠あり）

## インストール

```r
remotes::install_github("ShoFujihara/surveyMeta")
```

## APIキーの設定

このパッケージはLLM（大規模言語モデル）のAPIを利用します。APIとは、プログラムから外部のサービスを呼び出す仕組みです。Rのコードから直接LLMにリクエストを送り、結果を受け取ります（ChatGPTのようなチャット画面は使いません）。

### Claude（デフォルト、高精度）

**有料。** 調査1件あたり約$0.20（約30円）。新規アカウントに少額の無料クレジットあり。

1. [Anthropic Console](https://console.anthropic.com/) でアカウント作成
2. [API Keys](https://console.anthropic.com/settings/keys) で「Create Key」をクリック
3. Rで `.Renviron` にキーを保存:

```r
usethis::edit_r_environ()
# 以下の1行を追加して保存
# ANTHROPIC_API_KEY=sk-ant-api03-xxxxx
```

4. Rを再起動

### Gemini（無料枠あり）

**無料枠あり。** 1日1,500リクエストまで無料。有料でも調査1件あたり約$0.004（約0.6円）と非常に安価。

1. [Google AI Studio](https://aistudio.google.com/apikey) でAPIキーを取得
2. Rで `.Renviron` にキーを保存:

```r
usethis::edit_r_environ()
# 以下の1行を追加して保存
# GEMINI_API_KEY=AIzaSy...
```

3. Rを再起動

## クイックスタート：SSM2015調査票からメタデータを抽出

```r
library(surveyMeta)

# Claude（デフォルト）
result <- extract_metadata("2015SSM面接調査票.pdf")

# Gemini 2.0 Flash（無料枠で利用可能）
result <- extract_metadata("2015SSM面接調査票.pdf", model = "gemini-2.0-flash")

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

## 複数文書の同時入力

調査票と報告書を同時に入力すると、サンプルサイズや調査方法の記述が補完され精度が向上する。

```r
result <- extract_metadata(c(
  "2015SSM面接調査票.pdf",
  "2015SSM調査報告書.pdf"
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
export_metadata(result, "ssm2015_metadata.csv")
export_metadata(result, "ssm2015_metadata.xlsx", format = "xlsx")
export_metadata(result, "ssm2015_metadata.json", format = "json")
```

## バッチ処理

```r
surveys <- list(
  c("ssm2015_面接調査票.pdf", "ssm2015_報告書.pdf"),
  c("jgss2010_調査票.pdf"),
  c("nfrj2008_調査票.pdf", "nfrj2008_コードブック.pdf")
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
# モデル指定
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

## 抽出フィールド一覧（JDCatメタデータスキーマ対応）

| フィールド | JDCat対応 | 内容 | 型 |
|---|---|---|---|
| title | Title / タイトル ◎ | 調査名 | テキスト |
| survey_overview | Summary / 概要 ○ | 調査の概要（2-3文の要約） | テキスト |
| data_type | Data Type / データタイプ ○ | データタイプ | 統制語彙（enum） |
| data_language | Data Language / データの言語 ○ | 言語（ISO 639-3） | 統制語彙（enum） |
| survey_target | Universe / 母集団 △ | 母集団の定義 | テキスト |
| unit_of_analysis | Unit of Analysis / 観察単位 ○ | 調査単位 | DDI統制語彙（enum） |
| sample_size | Sampling Rate / 回収率 | 計画標本数・有効回答数・回収率 | オブジェクト |
| survey_date | Date of collection / 調査日 | 調査実施期間 | テキスト |
| reference_period | Time Period(s) / 対象時期 ○ | データ参照時期 | テキスト or null |
| geographic_coverage | Geographic Coverage / 対象地域 ○ | 地理的範囲 | テキスト |
| sampling_procedure | Sampling Procedure / サンプリング方法 △ | 標本抽出方法 | DDI統制語彙（配列） |
| mode_of_collection | Collection method / 調査方法 △ | データ収集方法 | DDI統制語彙（配列） |
| survey_conductor | Author / 作成者 ◎ | 調査実施機関 | テキスト |
| main_survey_items | - | 調査票の質問項目一覧 | 配列 |
| cessda_topic | Topic / トピック ○ | CESSDA Topic Classification v2.0 | 統制語彙（配列） |
| ssjda_topic | - | SSJDAオリジナルトピック | 統制語彙（配列） |
| evidence | - | 各フィールドの抽出根拠（原文引用＋ページ番号） | オブジェクト |

JDCat対応欄の記号: ◎必須 ○強く推奨 △推奨

## 引用

```r
citation("surveyMeta")
```

> Fujihara, S. (2026). surveyMeta: Extract Structured Metadata from Social Survey Documents Using LLM APIs. https://github.com/ShoFujihara/surveyMeta

## License

MIT
