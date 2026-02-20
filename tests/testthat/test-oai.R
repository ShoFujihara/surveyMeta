# Tests for oai.R (parse_ddi_xml)

test_that("parse_ddi_xml extracts survey_date from method block", {
  # Regression: collDate is inside <method>, not <stdyInfo>
  # The parser must search the method block, not the study description block
  xml <- '<?xml version="1.0"?>
<OAI-PMH>
  <GetRecord>
    <record>
      <metadata>
        <codeBook>
          <stdyDscr>
            <stdyInfo xml:lang="ja">
              <abstract xml:lang="ja">Test survey overview</abstract>
              <dataKind xml:lang="ja">量的調査: ミクロデータ</dataKind>
              <universe xml:lang="ja">全国の20歳以上</universe>
              <anlyUnit xml:lang="ja">個人</anlyUnit>
              <geogCover xml:lang="ja">日本</geogCover>
            </stdyInfo>
            <method xml:lang="ja">
              <dataColl>
                <collDate event="start" date="2020-01-15"/>
                <collDate event="end" date="2020-03-31"/>
                <sampProc xml:lang="ja">確率: 層別抽出</sampProc>
                <collMode xml:lang="ja">個別面接法</collMode>
              </dataColl>
            </method>
            <citation xml:lang="ja">
              <rspStmt>
                <AuthEnty xml:lang="ja">テスト機関</AuthEnty>
              </rspStmt>
            </citation>
          </stdyDscr>
        </codeBook>
      </metadata>
    </record>
  </GetRecord>
</OAI-PMH>'

  result <- parse_ddi_xml(xml, lang = "ja")

  # survey_date must be extracted (was NULL before fix)
  expect_false(is.null(result$survey_date))
  expect_true(grepl("2020-01-15", result$survey_date))
  expect_true(grepl("2020-03-31", result$survey_date))

  # Other fields should also work
  expect_equal(result$survey_overview, "Test survey overview")
  expect_equal(result$data_type, "量的調査: ミクロデータ")
  expect_equal(result$survey_target, "全国の20歳以上")
  expect_equal(result$geographic_coverage, "日本")
  expect_equal(result$sampling_procedure, "確率: 層別抽出")
  expect_equal(result$mode_of_collection, "個別面接法")
  expect_equal(result$survey_conductor, "テスト機関")
})

test_that("parse_ddi_xml handles missing collDate gracefully", {
  xml <- '<?xml version="1.0"?>
<OAI-PMH>
  <GetRecord>
    <record>
      <metadata>
        <codeBook>
          <stdyDscr>
            <stdyInfo xml:lang="ja">
              <abstract xml:lang="ja">No date survey</abstract>
            </stdyInfo>
            <method xml:lang="ja">
              <dataColl>
                <sampProc xml:lang="ja">確率: 単純無作為抽出</sampProc>
              </dataColl>
            </method>
          </stdyDscr>
        </codeBook>
      </metadata>
    </record>
  </GetRecord>
</OAI-PMH>'

  result <- parse_ddi_xml(xml, lang = "ja")
  expect_null(result$survey_date)
})

test_that("parse_ddi_xml extracts CESSDA topics", {
  xml <- '<?xml version="1.0"?>
<OAI-PMH>
  <GetRecord>
    <record>
      <metadata>
        <codeBook>
          <stdyDscr>
            <stdyInfo xml:lang="ja">
              <abstract xml:lang="ja">Topic test</abstract>
              <topcClas vocab="CESSDATopic">教育</topcClas>
              <topcClas vocab="CESSDATopic">健康</topcClas>
            </stdyInfo>
          </stdyDscr>
        </codeBook>
      </metadata>
    </record>
  </GetRecord>
</OAI-PMH>'

  result <- parse_ddi_xml(xml, lang = "ja")
  expect_length(result$cessda_topic, 2)
  expect_true("教育" %in% result$cessda_topic)
  expect_true("健康" %in% result$cessda_topic)
})
