# Ping Pong - Distributed Erlang on ESP32

A distributed Erlang application running on ESP32 microcontrollers using AtomVM. Two ESP32 boards communicate over WiFi and play ping-pong by exchanging messages and flashing LEDs.

## Prerequisites

### AtomVM Image (CRITICAL)

**You MUST flash your ESP32 with an AtomVM image built from the `main` branch**, not a release version. The distributed Erlang features required for this project are only available in the latest development version.

To build and flash AtomVM:

```bash
git clone https://github.com/atomvm/AtomVM.git
cd AtomVM
git checkout main
# Follow AtomVM build instructions for ESP32
# Flash the generated .img file to your ESP32
```

## Configuration

Edit `src/config.erl` to set your WiFi credentials:

```erlang
get() ->
  #{
    sta => [{ssid, "YourSSID"}, {psk, "YourPassword"}],
    port => 6969,
    led => #{pin => 2, flash_duration => 100}
   }.
```

## Build

```bash
make build
```

This compiles the Erlang code and creates an `.avm` package file at:
`_build/default/lib/iot_node.avm`

## Flash

Flash both ESP32 boards with the compiled application:

```bash
# Flash both ESP32 at once
make flash

# Or flash individually
make flash1  # Flash ESP32 #1 on /dev/tty.usbserial-0001
make flash2  # Flash ESP32 #2 on /dev/tty.usbserial-3
```

**Note**: Update the `PORT1` and `PORT2` variables in `Makefile` to match your USB serial ports.

## How It Works

1. Both ESP32 boards boot and connect to WiFi
2. Each board starts distributed Erlang with a unique node name based on its IP:
   - `atomvm@192.168.1.49`
   - `atomvm@192.168.1.50`
3. After a 5-second stabilization period, the `.50` node sends the first `ping`
4. The nodes exchange `ping`/`pong` messages indefinitely
5. Each message reception triggers an LED flash

## Network Requirements

- Both ESP32 must be on the same WiFi network
- The application expects IP addresses:
  - `192.168.1.49` for one board
  - `192.168.1.50` for the other
- If your DHCP assigns different IPs, update the logic in `src/iot_node.erl:65-68`

## Troubleshooting

### "invalid_challenge" or connection errors
- Ensure both ESP32 are running the same code version
- Check that both boards have the same cookie (`AtomVM`)
- Verify network connectivity between the boards

### Application terminates immediately
- Make sure you're using AtomVM from `main` branch, not a release
- Check that WiFi credentials are correct in `config.erl`

## Manual Testing

You can also trigger the ping-pong from an Erlang shell:

```erlang
% Connect to the node
erlang:set_cookie('atomvm@192.168.1.50', 'AtomVM').

% Send initial ping
{ping_pong, 'atomvm@192.168.1.50'} ! ping.

% To stop
{ping_pong, 'atomvm@192.168.1.50'} ! stop.
```
