#!/bin/bash
# UI スモークテスト: 全シナリオを実際の画面で数ターン進め、画面が固まらず、メッセージが暴走しないことを確かめる。
# (FuncUI が部品を位置で使い回すと、スライダーの変更イベントが誤ったメッセージを発行して再描画が無限に続く、
#  という不具合があったため。ユニットテストでは検出できない)
#
# 使い方:  tools/ui-smoke.sh [発行済みアプリのディレクトリ]   (省略すると dotnet publish してから走らせる)
# 必要なもの: Xvfb, xdotool, ImageMagick (import)
set -u
ROOT="$(cd "$(dirname "$0")/.." && pwd)"
APP="${1:-}"
if [ -z "$APP" ]; then
  APP="$(mktemp -d)"
  dotnet publish "$ROOT/src/WaterGame.App" -c Release -o "$APP" >/dev/null 2>&1 || { echo "publish failed"; exit 2; }
fi
DN=${DN:-197}
export DISPLAY=:$DN
LOG="$(mktemp)"
fail=0
MAX_MSGS_PER_TURN=60    # 1ターンで発行されるメッセージの上限 (スライダーの初期化イベントと、右パネルを数か所クリックした分を含む。暴走すると数千件になる)
TURNS=${TURNS:-5}

(Xvfb :$DN -screen 0 1280x860x24 >/dev/null 2>&1 &)
sleep 1

for scenario in チュートリアル 平年 渇水 市街地拡大 乾燥地 長期の試練; do
  : > "$LOG"
  (cd "$APP" && WATERGAME_TRACE=1 dotnet WaterGame.App.dll --scenario "$scenario" --seed 16 --skip-intro --funds 9000 > "$LOG" 2>&1 & echo $! > "$LOG.pid")
  sleep 9
  ok=1
  for t in $(seq 1 $TURNS); do
    before=$(grep -c '\[msg\]' "$LOG")
    # 右の盤の数か所 (バルブ・ダイヤル・レバー・タブ) をクリックして、部品の使い回しによる暴走が起きないかを見る
    # (窓は Xvfb の左上から (10,10) ずれて出る。盤面は 1180x800)
    for p in "400,380" "520,380" "640,380" "1100,380" "1050,300" "700,640" "750,88" "640,88" "424,88"; do
      xdotool mousemove ${p%,*} ${p#*,} click 1; sleep 0.4
    done
    xdotool mousemove 750 776 click 1; sleep 3
    after=$(grep -c '\[msg\]' "$LOG")
    delta=$((after - before))
    if [ "$delta" -gt "$MAX_MSGS_PER_TURN" ]; then
      echo "FAIL  $scenario: ターン$t でメッセージが $delta 件発行された (暴走の疑い)"; ok=0; break
    fi
    # 日報のページを送ってから (次へ)、ナイフスイッチ「次の季節へ」で閉じる
    xdotool mousemove 556 745 click 1; sleep 0.6
    xdotool mousemove 715 745 click 1; sleep 1
  done
  if [ "$ok" = 1 ]; then
    cpu=$(ps -o pcpu= -p "$(cat "$LOG.pid")" | tr -d ' ')
    echo "PASS  $scenario: $TURNS ターン進行 (最終のCPU ${cpu}%)"
  else
    fail=1
  fi
  kill "$(cat "$LOG.pid")" 2>/dev/null
  sleep 1
done
pkill -f "Xvfb :$DN " 2>/dev/null
exit $fail
