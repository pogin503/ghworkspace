#!/usr/bin/env bash
# tap.sh — テスト用 TAP デバイスの設定・解除スクリプト
#
# 使い方:
#   sudo ./scripts/tap.sh up   [DEV]   TAP デバイスを作成して IP を設定する
#   sudo ./scripts/tap.sh down [DEV]   TAP デバイスを解除・削除する
#        ./scripts/tap.sh status [DEV] デバイスの現在の状態を表示する
#
# DEV のデフォルト: tap0
# ホスト側 IP:     10.0.0.1/24  (src/main.c の STACK_IP と同じサブネット)
# スタック IP:     10.0.0.2     (src/main.c の STACK_IP)

set -euo pipefail

# --- 設定値 (src/main.c と一致させること) ---
DEV="${2:-tap0}"
HOST_IP="10.0.0.1"
HOST_PREFIX="24"
STACK_IP="10.0.0.2"

# --- ヘルパー関数 ---

usage() {
    cat <<EOF
使い方: $(basename "$0") <コマンド> [DEV]

コマンド:
  up      TAP デバイスを作成して UP する (要 root)
  down    TAP デバイスを DOWN して削除する (要 root)
  status  TAP デバイスの現在の状態を表示する

DEV のデフォルト: tap0

例:
  sudo $0 up
  sudo $0 up tap1
  $0 status
  sudo $0 down
EOF
}

# デバイスが存在するか確認
dev_exists() {
    ip link show "$DEV" > /dev/null 2>&1
}

# root 権限が必要なコマンドの前に確認
require_root() {
    if [[ "${EUID:-$(id -u)}" -ne 0 ]]; then
        echo "エラー: このコマンドは root 権限が必要です。" >&2
        echo "  sudo $0 $*" >&2
        exit 1
    fi
}

# --- サブコマンド ---

cmd_up() {
    require_root up "$@"

    if dev_exists; then
        echo "情報: $DEV は既に存在します。スキップします。"
        ip addr show "$DEV"
        return 0
    fi

    echo "==> TAP デバイス '$DEV' を作成します..."
    ip tuntap add dev "$DEV" mode tap
    echo "    ip tuntap add dev $DEV mode tap ... OK"

    ip addr add "${HOST_IP}/${HOST_PREFIX}" dev "$DEV"
    echo "    ip addr add ${HOST_IP}/${HOST_PREFIX} dev $DEV ... OK"

    ip link set "$DEV" up
    echo "    ip link set $DEV up ... OK"

    echo ""
    echo "==> 設定完了"
    ip addr show "$DEV"
    echo ""
    echo "スタック側 IP : $STACK_IP"
    echo "ホスト側 IP   : $HOST_IP/$HOST_PREFIX"
    echo ""
    echo "起動コマンド: sudo bin/tcp_stack $DEV"
}

cmd_down() {
    require_root down "$@"

    if ! dev_exists; then
        echo "情報: $DEV は存在しません。スキップします。"
        return 0
    fi

    echo "==> TAP デバイス '$DEV' を削除します..."
    ip link set "$DEV" down
    echo "    ip link set $DEV down ... OK"

    ip tuntap del dev "$DEV" mode tap
    echo "    ip tuntap del dev $DEV mode tap ... OK"

    echo "==> '$DEV' を削除しました。"
}

cmd_status() {
    if ! dev_exists; then
        echo "$DEV: 存在しません"
        return 0
    fi

    echo "=== $DEV の状態 ==="
    ip link show "$DEV"
    echo ""
    ip addr show "$DEV"
}

# --- エントリーポイント ---

CMD="${1:-}"

case "$CMD" in
    up)     cmd_up ;;
    down)   cmd_down ;;
    status) cmd_status ;;
    ""|--help|-h) usage; exit 0 ;;
    *)
        echo "エラー: 不明なコマンド '$CMD'" >&2
        echo "" >&2
        usage >&2
        exit 1
        ;;
esac
