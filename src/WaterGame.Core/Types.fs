namespace WaterGame.Core

/// 水量単位: 1 WU = 1,000 m3。1 mm x 1 km2 = 1 WU。
type Season =
    | Spring
    | Summer
    | Autumn
    | Winter

/// 河川は 森林 -> 農地 -> 市街地・工業 -> 湿地 の順に流れる
type ZoneId =
    | Forest
    | Farm
    | Urban
    | Wetland

type EtLevel =
    | Low
    | Mid
    | High

type Allocatee =
    | Agriculture
    /// 生活・産業用水 (初版では生活用水と産業用水をまとめて扱う)
    | Municipal
    | Environment

/// 地下水調査の種類 (仕様書 8.1)
type SurveyKind =
    | Reconnaissance
    | TestDrilling

/// 調査状態。井戸建設状態とは別の判別共用体で持つ (建設しても調査情報は失われない)
type SurveyStage =
    | Unsurveyed
    | Reconnoitered
    | TestDrilled

type WellState =
    | NoWell
    | WellBuilt

/// 設備投資。灌漑効率改善・貯水池・観測井戸・雨水利用・再生水・節水キャンペーン (浸透改善は、地下水の指標が頭打ちで点数に響く経路がなかったため廃止した)
type Investment =
    /// 同じ生産に必要な正味灌漑需要を減らす (水路の改修)
    | IrrigationEfficiency
    /// 灌漑効率の拡張 (高効率の灌漑方式)。灌漑効率の改善のあとで実施できる
    | IrrigationUpgrade
    /// 対象区域の地下水位を継続して観測し、推定の誤差を季節ごとに縮める
    | ObservationWell of ZoneId
    /// 取水点の上流に貯水池をつくる (小規模)。季節をまたいで河川水を貯め、放流できる
    | Reservoir
    /// 貯水池の拡張 (容量を増やす)。貯水池の建設のあとで実施できる
    | ReservoirExpansion
    /// 市街地の雨水利用施設。生活・産業用水の需要を減らす
    | RainwaterHarvest
    /// 下水の再生水を河川へ戻す。供給した生活・産業用水の一部が下流の流量に加わる
    | EffluentReuse
    /// 再生水の処理能力の拡張 (戻す割合が増える)。再生水の放流のあとで実施できる
    | EffluentUpgrade
    /// 節水キャンペーン。農業・生活産業の需要を、少し減らす
    | WaterSaving

/// 資材の区分 (v2.0-c)。個々の品目や輸送は扱わず、3つに抽象化する
type Material =
    /// 機械設備 (ポンプ・主要機械)
    | Machinery
    /// 建設資材 (鉄管・セメントなど)
    | BuildMaterial
    /// 専門技術者 (工事のあいだ、その工事につきっきりになる)
    | Engineer

/// 1ターンに実行できる主要行動 (調査・井戸建設・設備投資・資材の融通のいずれか1つ)
type MajorAction =
    | NoAction
    | Survey of ZoneId * SurveyKind
    | BuildWell of ZoneId
    | Invest of Investment
    /// 他地域から、資材を1単位、融通してもらう (すぐ届くが、あとで返す)
    | RequestTransfer of Material

/// 工事に必要な資材の量 (単位)
type MaterialNeeds =
    { Machinery: int
      BuildMaterial: int
      Engineers: int }

type ProjectStatus =
    /// 必要な資材がそろうのを待っている
    | Waiting
    /// 施工中
    | Building

/// 建設プロジェクト。発注 (主要行動) で作られ、資材がそろうと着工し、標準工期のあと完成する。
/// 完成した季節の翌季から、効果が出る。
type Project =
    { Id: int
      /// 完成すると実行される行動 (井戸の建設か、設備投資)
      Target: MajorAction
      Needs: MaterialNeeds
      /// 標準工期 (季節)。着工した季節を1と数える
      Duration: int
      OrderedTurn: int
      Status: ProjectStatus
      StartedTurn: int option
      /// 着工後の残りの季節
      Remaining: int }

/// 融通で借りた資材 1 単位。返礼期限のターンに、正規配給から差し引かれる
type TransferLoan =
    { Material: Material
      RepayTurn: int }

/// 資材・建設のシナリオ設定 (v2.0-c)。None のシナリオ (入門) では、工事は、発注した季節に完成する
type ConstructionConfig =
    { /// 開始時の資材 (専門技術者は、人数)
      InitialStock: Map<Material, int>
      /// 正規配給の日程 (到着するターン, 資材, 量)。専門技術者の増員もここに書く
      Arrivals: (int * Material * int) list
      /// 融通の手数料
      TransferFee: float
      /// 融通で借りた資材を返すまでの季節数
      TransferRepayAfter: int
      /// 同時に借りられる上限 (単位)
      MaxLoans: int }

/// 建設・資材の状態
type ConstructionState =
    { Stock: Map<Material, int>
      Projects: Project list
      Loans: TransferLoan list
      /// 次に発注される工事の番号
      NextProjectId: int }

/// 1ターンの、建設・資材の動き
type WorkTurn =
    { /// 今季に発注された工事
      Ordered: Project option
      /// 今季に着工した工事
      Started: Project list
      /// 今季に完成した工事 (効果は、翌季から)
      Completed: Project list
      /// 融通で資材を借りた
      Transferred: Material option
      /// 返礼で差し引かれた資材
      Repaid: Material list
      /// 来季のはじめに届く正規配給
      ArrivingNext: (Material * int) list
      /// 今季の終わりの、各資材の量
      StockAfter: Map<Material, int> }

/// 区域のシナリオデータ。初版ではプレイヤーが直接変更しない。
type ZoneParams =
    { Id: ZoneId
      Name: string
      AreaKm2: float
      /// 降水のうち地表流出になる割合 (残りは土壌へ浸透)
      RunoffCoef: float
      SoilCapacityMm: float
      /// 蒸発散需要に掛ける土地被覆係数
      EtFactor: float
      InitialSoilMm: float
      /// 浸透した降水のうち、土壌に留まらず直接、地下水に入る割合 (岩盤の割れ目や砂礫層を通る涵養)
      RechargeFrac: float }

/// 帯水層の公開情報。初期貯留量と持続的揚水量は範囲だけが公開され、真の値はシードから決まる隠しパラメータ。
type AquiferSpec =
    { Zone: ZoneId
      /// 初期地下水貯留量の範囲 (WU)
      InitialGRangeWU: float * float
      /// 持続的揚水量の範囲 (WU/季節)
      YieldRangeWU: float * float
      /// 地下水から河川への湧出率 (季節あたり、貯留量に対する割合)
      DischargeK: float
      /// 井戸1本の揚水能力 (WU/季節)
      WellCapacityWU: float
      /// この貯留量を下回ると揚水コスト上昇フラグが有効になる (WU)
      ThresholdWU: float
      /// フラグ有効後の揚水コスト倍率
      CostMultiplier: float
      /// 河川から地下水への浸透率 (その区域に流れ込む河川水に対する割合の上限)。地下水位が下がるほど効く。0 なら浸透しない
      RiverSeepageK: float
      // ---- 隠しの真の値。シナリオごとに固定で、乱数では決めない (運の要素は、気象だけにする) ----
      /// 初期地下水貯留量の真の値が、範囲 (InitialGRangeWU) のどのあたりか (0〜1)
      TrueInitialGFrac: float
      /// 持続的揚水量の真の値が、範囲 (YieldRangeWU) のどのあたりか (0〜1)
      TrueYieldFrac: float
      /// 概査の推定誤差の向き [-1, 1] (誤差の大きさは調査段階の相対誤差を掛ける)
      ReconBias: float
      /// 試掘・揚水試験の推定誤差の向き [-1, 1]
      DrillBias: float }

/// 費用のパラメータ。
type CostParams =
    { ReconCost: float
      TestDrillCost: float
      WellCost: float
      PumpCostPerWU: float
      /// 持続的揚水量を超えた分の揚水費にかかる倍率
      OverdraftFactor: float
      /// 調査後の推定の相対誤差 (±)
      ReconRelErr: float
      TestDrillRelErr: float
      /// 真の持続的揚水量を超えた揚水の累積 (超過の借り) が、持続的揚水量のこの倍率に達すると、揚水コスト上昇フラグが有効になる
      OverdraftLimit: float }

/// 財政のパラメータ (仕様書 10.1)。単一勘定で、借入・利子は扱わない。
type FinanceParams =
    { InitialBalance: float
      /// 毎年(春ターンの開始時)に受け取る管理交付金
      AnnualGrant: float
      /// 農業生産(農業用水の供給実績)1 WU あたりの収入
      AgRevenuePerWU: float
      /// 生活・産業用水の供給実績 1 WU あたりの料金収入
      UrbanRevenuePerWU: float
      /// 井戸1本あたりの維持管理費 (季節ごと)
      WellMaintenance: float }

type InvestParams =
    { IrrigationCost: float
      /// 灌漑効率改善による正味灌漑需要の低減割合
      IrrigationDemandReduction: float
      IrrigationUpgradeCost: float
      /// 拡張後の、正味灌漑需要の低減割合 (改善の分を含む合計)
      IrrigationUpgradeReduction: float
      ObservationCost: float
      /// 観測井戸が置かれた区域で、季節ごとに推定の誤差が縮む割合 (残る誤差 = 1 - この値)
      ObservationShrink: float
      /// 観測井戸で縮められる誤差の下限
      ObservationFloor: float
      /// 観測井戸のある区域の揚水費の低減割合 (水位を見ながらの効率的な運転)
      ObservationPumpSaving: float
      RainwaterCost: float
      /// 雨水利用による生活・産業用水の需要の低減割合
      RainwaterDemandReduction: float
      EffluentCost: float
      /// 再生水として河川へ戻る、河川水による生活・産業用水の供給量の割合
      EffluentReturnRate: float
      EffluentUpgradeCost: float
      /// 拡張後に、河川へ戻る割合
      EffluentUpgradeReturnRate: float
      WaterSavingCost: float
      /// 節水キャンペーンによる、農業・生活産業の需要の低減割合
      WaterSavingDemandReduction: float }

/// 貯水池のパラメータ。None のシナリオでは建設できない (入門)
type ReservoirParams =
    { Cost: float
      /// 貯留容量 (WU)。建設した直後 (小規模) の値
      CapacityWU: float
      ExpansionCost: float
      /// 拡張後の貯留容量 (WU)
      ExpandedCapacityWU: float
      /// 季節ごとの蒸発損失率 (季節開始時の貯留量に対する割合)。夏が大きい
      EvapRate: Map<Season, float> }

/// 季節ごとのできごとの種類。予告され、その季節だけ効く
type EventKind =
    /// 管理交付金に掛ける倍率 (春だけ意味を持つ)
    | GrantMultiplier of float
    /// 臨時の補助金 (残高に加わる)
    | ExtraFunds of float
    | AgDemandMultiplier of float
    | UrbanDemandMultiplier of float
    /// 全ての井戸の揚水能力に掛ける倍率 (停電・点検)
    | PumpCapacityMultiplier of float
    /// 揚水費に掛ける倍率 (電力料金の高騰)
    | PumpCostMultiplier of float

/// できごとのカード。シナリオの日程 (EventSchedule) で、起こるターンが固定で決まる
type EventCard =
    { Name: string
      Description: string
      Kind: EventKind }

/// 評価の重み。シナリオによって重視する目標を変える (仕様書 10.2)。
type EvalSettings =
    { AgWeight: float
      UrbanWeight: float
      EnvWeight: float
      GwWeight: float
      FinWeight: float
      /// 財政の健全性の基準となる最終残高
      TargetBalance: float
      /// 地下水の貯留量が「健全」とみなす終了時/開始時の比。何もしなくても自然な湧出で貯留量は減るため、
      /// その分は許容する (これ以上なら満点)
      GwReferenceRatio: float }

/// 帯水層の隠しパラメータ (真の値)。UI には出さない。シナリオから決まる (乱数なし)。
type HiddenAquifer =
    { Zone: ZoneId
      InitialG: float
      Yield: float
      /// 調査結果の推定誤差の向き [-1, 1] (誤差の大きさは調査段階の相対誤差を掛ける)
      ReconBias: float
      DrillBias: float }

/// プレイヤーが知っている情報に基づく地下水の推定値
type Estimate =
    { GwWU: float
      YieldWU: float
      /// 推定の相対誤差 (±)
      RelErr: float }

/// 気象カード。降水量は予報として範囲で公開し、実績値はこの範囲内で決まる。
type WeatherCard =
    { Name: string
      Season: Season
      PrecipMm: float * float
      EtLevel: EtLevel
      Weight: float
      /// このカードが引かれうるターンの範囲 (0 始まり、両端を含む)。前半と後半で気象の傾向を変えるために使う
      FromTurn: int
      ToTurn: int }

/// ゲーム開始時に確定する気象実績。プレイヤーには結果カードで初めて公開する。
type ActualWeather =
    { Turn: int
      Season: Season
      Card: WeatherCard
      PrecipMm: float
      /// 基準蒸発散需要 (土地被覆係数を掛ける前)
      EtDemandMm: float }

/// 計画指令の種類 (中央からの要求)
type DirectiveKind =
    /// 生産ノルマ (綿花の増産): 期間の農業用水の充足率を、目標以上にする
    | CottonQuota
    /// 工業用水の供給: 期間の生活・産業用水の充足率を、目標以上にする
    | IndustrialSupply
    /// 建設期限: 期限までに、指定の設備を完成させる (資材があっても、工期に間に合わないと未達)
    | BuildOrder

/// 計画指令。シナリオから固定で決まり (乱数なし)、発令のターンに示される
type Directive =
    { Id: int
      Kind: DirectiveKind
      /// 発令のターン (0 始まり)。この季節のはじめに示される
      IssuedTurn: int
      /// 期限のターン (0 始まり)。このターンの結果まで含めて判定する
      DeadlineTurn: int
      /// 期間の充足率 (供給量の合計 / 需要量の合計) の目標。建設期限では 1
      TargetFill: float
      /// 建設期限で、完成させる設備 (井戸の建設か、設備投資)
      Target: MajorAction option }

type DirectiveOutcome =
    | Achieved
    /// 目標に近いが届かない。信頼は動かない
    | Partial
    | Failed

type DirectiveResult =
    { Directive: Directive
      /// 期間の充足率
      Measured: float
      /// 充足率 / 目標
      Ratio: float
      Outcome: DirectiveOutcome
      TrustChange: float
      /// 達成報酬の補助金
      Reward: float }

/// 年次報告の方針。冬の季節 (年の最後) に、その年に期限が来た指令の結果を、中央へ報告する
type ReportStance =
    /// 実際の結果を、そのまま報告する
    | Accurate
    /// 結果を、実際より一段よく見せる (未達→一部達成、一部達成→達成)
    | Optimistic
    /// 未達の指令を、報告から外す
    | Conceal

/// 報告に含まれた指令1件。実際の判定と、報告した判定を並べて持つ (実値と報告値)
type ReportItem =
    { DirectiveId: int
      /// 実際の判定
      Actual: DirectiveOutcome
      /// 報告した判定。None は、報告から外したこと
      Reported: DirectiveOutcome option
      /// 報告によって、上司の信頼に加わった分 (実際のとおりなら 0)
      TrustGain: float
      /// 報告によって加わった補助金 (実際のとおりなら 0)
      RewardGain: float }

/// 年次報告の記録
type ReportRecord =
    { Turn: int
      Stance: ReportStance
      Items: ReportItem list
      /// この報告で増えた整合性リスク
      RiskDelta: float }

/// 監査の記録。虚偽の報告が残っていれば露見し、なければ問題なし
type AuditRecord =
    { Turn: int
      /// 露見した虚偽の報告
      Exposed: ReportItem list
      /// 上司の信頼の変化 (露見で下がる。問題なしなら少し上がる)
      TrustChange: float
      /// 予算削減 (罰金)
      Fine: float }

/// 計画指令の設定 (シナリオごと)。None のシナリオ (入門) には、指令がない
type DirectiveConfig =
    { /// 目標の充足率の範囲
      TargetMin: float
      TargetMax: float
      /// 達成報酬の補助金 = 年度の交付金 × この値
      RewardFraction: float
      TrustAchieved: float
      TrustFailed: float
      InitialTrust: float }

type Scenario =
    { Name: string
      /// 難易度の区分 (入門 / 標準 / 特殊 / 高難度)
      Level: string
      /// メニューに出す短い説明
      Summary: string
      /// シナリオが重視する目標の説明
      Focus: string
      /// ターン(0 始まり)ごとのヒント。チュートリアルで、そのターンに何をするとよいかを案内する
      Hints: Map<int, string>
      /// 年ごとの生活・産業用水の需要の伸び (複利)。市街地拡大で使う
      UrbanDemandGrowthPerYear: float
      /// 年ごとの市街地の流出係数の増加 (舗装の拡大)。上限 0.9
      UrbanRunoffGrowthPerYear: float
      Turns: int
      /// 上流から下流の順
      Zones: ZoneParams list
      /// 河川水の配分管理地点。この区域の流出までを配分対象とし、残りの区域は配分地点より下流で河川に加わる
      AllocationAfter: ZoneId
      Cards: WeatherCard list
      /// 季節ごとの農業用水の正味需要 (WU)
      AgDemandWU: Map<Season, float>
      /// 季節ごとの生活・産業用水の需要 (WU)
      UrbanDemandWU: Map<Season, float>
      /// 季節ごとの環境流水の最低基準 (WU)。下流管理地点の季節流量で評価する
      MinEnvFlowWU: Map<Season, float>
      /// 季節ごとの基準蒸発散需要 (mm)
      BaseEtMm: Map<Season, float>
      Aquifers: AquiferSpec list
      Costs: CostParams
      Finance: FinanceParams
      Invest: InvestParams
      Reservoir: ReservoirParams option
      /// できごとの日程 (ターン(0 始まり), カード)。固定で、乱数では決めない。季節のはじめに予告される
      EventSchedule: (int * EventCard) list
      /// 計画指令 (中央からのノルマ)。None なら指令がない
      Directives: DirectiveConfig option
      /// 資材・建設 (v2.0-c)。None なら、工事は発注した季節に完成する
      Construction: ConstructionConfig option
      Eval: EvalSettings }

/// プレイヤーが1ターンに決める政策。
type Policy =
    { AgTargetWU: float
      /// 生活・産業用水を河川水から供給したい量
      UrbanTargetWU: float
      EnvTargetWU: float
      /// 配分優先順位 (3項目の並び。春ターンのみ変更が反映される)
      Priority: Allocatee list
      /// 区域ごとの地下水揚水上限 (WU/季節)
      PumpLimitWU: Map<ZoneId, float>
      /// 貯水池の操作 (WU)。正なら河川水を貯め、負なら貯水池から放流する。貯水池がなければ 0 に丸める
      ReservoirOpWU: float
      Action: MajorAction
      /// 年次報告の方針。冬の季節 (年の最後) だけ効く。計画指令のないシナリオでは無視される
      Report: ReportStance }

type ZoneResult =
    { Zone: ZoneId
      PrecipWU: float
      RunoffWU: float
      InfiltrationWU: float
      EtDemandWU: float
      ActualEtWU: float
      /// 深部排水 = 地下水涵養量 R
      DeepDrainageWU: float
      SoilStartWU: float
      SoilEndWU: float
      SoilCapacityWU: float
      /// 季節開始時の地下水貯留量 G(t)
      GwStartWU: float
      /// 地下水から河川への湧出量 B(t) (G(t) から計算する)
      DischargeWU: float
      /// 揚水量 P(t)
      PumpedWU: float
      /// 季節終了時の地下水貯留量 G(t+1) = G(t) + R - P - B
      GwEndWU: float
      /// 河川に加わる量 (地表流出 + 地下水湧出)
      RiverContributionWU: float
      /// 河川から地下水へ浸透した量 (区域に流れ込む河川水から失われ、地下水に加わる)
      SeepageWU: float
      ResidualWU: float }

type AllocationResult =
    { /// 配分管理地点に到達した利用可能水量
      AvailableWU: float
      /// 河川水による農業用水の供給量
      AgSupplyWU: float
      /// 河川水による生活・産業用水の供給量
      UrbanSupplyWU: float
      /// 環境流水として河川に残すために予約した量 (用水としては供給しない)
      EnvReserveWU: float
      /// 配分地点より下流の区域から加わる量
      BelowContributionWU: float
      /// 再生水として下流へ戻る量 (生活・産業用水の河川水の供給量の一部)
      ReturnFlowWU: float
      /// 下流管理地点の季節流量
      DownstreamFlowWU: float }

/// 貯水池の今季の動き
type ReservoirResult =
    { StartWU: float
      /// 蒸発による損失
      EvaporationWU: float
      /// 河川から貯めた量
      StoredWU: float
      /// 貯水池から河川へ放流した量
      ReleasedWU: float
      EndWU: float
      CapacityWU: float }

type BalanceResult =
    { PrecipInWU: float
      ActualEtWU: float
      /// モデル域外へ供給した用水 (河川水 + 地下水)
      ExternalSupplyWU: float
      /// 下流端から流域外へ出た河川流量
      OutflowWU: float
      /// 土壌水分と地下水の貯留量の増加
      StorageChangeWU: float
      ResidualWU: float
      ToleranceWU: float }
    member b.IsWithinTolerance = abs b.ResidualWU <= b.ToleranceWU

type TurnCosts =
    { Survey: float
      Build: float
      Invest: float
      /// 井戸の維持管理費
      Maintenance: float
      Pumping: float }
    member c.Total = c.Survey + c.Build + c.Invest + c.Maintenance + c.Pumping

/// 調査結果の開示
type SurveyReport =
    { Zone: ZoneId
      Kind: SurveyKind
      GwEstWU: float
      YieldEstWU: float
      PrevRelErr: float
      RelErr: float }

/// 1ターンの、指令まわりの動き
type DirectiveTurn =
    { /// 今季はじめに示された指令
      Issued: Directive list
      /// 今季が期限で、判定された指令
      Resolved: DirectiveResult list
      TrustBefore: float
      TrustAfter: float
      /// 達成報酬の補助金の合計 (年次報告で加わった分を含む。期末残高に加わる)
      Reward: float
      /// 今季に提出した年次報告 (冬の季節で、報告する指令があるときだけ)
      Report: ReportRecord option
      /// 今季に入った監査
      Audit: AuditRecord option
      /// 監査による予算削減 (期末残高から引く)
      Fine: float
      RiskBefore: float
      RiskAfter: float
      /// 監査の予兆。true なら、次の季節に監査が入る
      AuditNotice: bool }

type TurnResult =
    { Turn: int
      Season: Season
      Weather: ActualWeather
      Policy: Policy
      EffectivePriority: Allocatee list
      Zones: ZoneResult list
      Allocation: AllocationResult
      /// 貯水池の動き (貯水池がなければ None)
      ReservoirFlow: ReservoirResult option
      /// 今季のできごと
      Event: EventCard option
      /// 観測井戸による推定の更新 (区域, 更新前の相対誤差, 更新後の相対誤差)
      Observations: (ZoneId * float * float) list
      AgDemandWU: float
      UrbanDemandWU: float
      /// 地下水で補った用水の合計 (農業 + 生活・産業)
      GwSupplyWU: float
      AgGwSupplyWU: float
      UrbanGwSupplyWU: float
      EnvTargetWU: float
      EnvMinWU: float
      /// 農業用水の充足率 (河川水 + 地下水)
      AgFillRate: float
      /// 生活・産業用水の充足率 (河川水 + 地下水)
      UrbanFillRate: float
      EnvAchievement: float
      EnvMinMet: bool
      Balance: BalanceResult
      Costs: TurnCosts
      /// 今季に受け取った管理交付金 (春のみ)
      Grant: float
      /// 農業生産に比例する収入
      Revenue: float
      BalanceStart: float
      BalanceEnd: float
      /// 実行された主要行動 (無効な行動は NoAction になる)
      ActionTaken: MajorAction
      SurveyReports: SurveyReport list
      /// このターンで揚水コスト上昇フラグが有効になった区域
      NewCostFlags: ZoneId list
      /// プレイヤーに見せる推定値 (このターン終了後)
      EstimatesAfter: Map<ZoneId, Estimate>
      /// 隠しの真の地下水貯留量 (このターン終了後)。デブリーフと検証用で、プレイ中は表示しない
      ActualGwAfter: Map<ZoneId, float>
      /// 学習目標の番号と1行解説
      ExplanationGoal: int
      Explanation: string
      Warnings: string list
      /// 計画指令まわりの動き (指令のないシナリオでは、すべて空)
      Plan: DirectiveTurn
      /// 建設・資材の動き (資材・建設のないシナリオでは、すべて空)
      Works: WorkTurn }

/// 組織・政治の層の状態。水文学の状態とは別に持ち、水量には影響しない
type OrganizationState =
    { /// ゲーム開始時に確定した、全期間の指令
      Directives: Directive list
      /// 上司の信頼 (0〜100)
      Trust: float
      Resolved: DirectiveResult list
      /// 整合性リスク (0〜100)。虚偽の報告で上がり、残っているあいだ毎季節上がる。高いと監査が入る
      Risk: float
      /// 報告済みの指令の番号
      Reported: Set<int>
      /// まだ露見していない虚偽の報告
      Misreports: ReportItem list
      /// 次の季節に監査が入る (予兆が出ている)
      AuditDue: bool
      Reports: ReportRecord list
      Audits: AuditRecord list }

/// 計画達成度と政治的評価のまとめ
type PlanSummary =
    { /// 判定済みの指令の達成率の平均 (判定済みがなければ None)
      Achievement: float option
      Results: DirectiveResult list
      /// 上司の信頼 (最終の監査を反映した値)
      Trust: float
      /// 整合性リスク
      Risk: float
      Reports: ReportRecord list
      Audits: AuditRecord list
      /// 終了時の監査。虚偽の報告が残っていて、整合性リスクが閾値以上のときだけ入る
      FinalAudit: AuditRecord option
      /// 最後まで露見しなかった虚偽の報告
      Undetected: ReportItem list }

type GameState =
    { Turn: int
      SoilWU: Map<ZoneId, float>
      /// 真の地下水貯留量 (隠し)
      GwWU: Map<ZoneId, float>
      Hidden: Map<ZoneId, HiddenAquifer>
      Survey: Map<ZoneId, SurveyStage>
      Wells: Map<ZoneId, WellState>
      Estimates: Map<ZoneId, Estimate>
      /// 揚水コスト上昇フラグ。一度有効になったら解除しない
      CostFlags: Map<ZoneId, bool>
      /// 真の持続的揚水量を超えた揚水の累積 (WU)。超えた分だけ増え、下回った分だけ減る (0 未満にはならない)
      Overdraft: Map<ZoneId, float>
      CostTotals: TurnCosts
      /// 財政の残高。年度末の残高は翌年度へ繰り越す
      Balance: float
      /// 実施済みの設備投資 (効果は実施した季節の翌季から)
      Invested: Set<Investment>
      Priority: Allocatee list
      /// 貯水池の貯留量 (貯水池がなければ 0)
      ReservoirWU: float
      /// ターンごとのできごと (シナリオの日程どおり)
      Events: Map<int, EventCard>
      Organization: OrganizationState
      Construction: ConstructionState
      History: TurnResult list }

module Season =
    let ofTurn (turn: int) =
        match turn % 4 with
        | 0 -> Spring
        | 1 -> Summer
        | 2 -> Autumn
        | _ -> Winter

    let label =
        function
        | Spring -> "春"
        | Summer -> "夏"
        | Autumn -> "秋"
        | Winter -> "冬"

module ZoneId =
    let label =
        function
        | Forest -> "上流山地・森林"
        | Farm -> "中流農地"
        | Urban -> "市街地・工業地域"
        | Wetland -> "下流湿地・河川環境"

module EtLevel =
    let label =
        function
        | Low -> "低"
        | Mid -> "中"
        | High -> "高"

module Allocatee =
    let label =
        function
        | Agriculture -> "農業用水"
        | Municipal -> "生活・産業用水"
        | Environment -> "環境流水"

module SurveyStage =
    let label =
        function
        | Unsurveyed -> "未調査"
        | Reconnoitered -> "概査済み"
        | TestDrilled -> "試掘・揚水試験済み"

module SurveyKind =
    let label =
        function
        | Reconnaissance -> "概査"
        | TestDrilling -> "試掘・揚水試験"
