PORT1 = /dev/tty.usbserial-0001
PORT2 = /dev/tty.usbserial-3

FLASH_CMD = rebar3 atomvm esp32_flash --port
BUILD_CMD = rebar3 atomvm packbeam

.PHONY: build flash flash1 flash2

build:
	$(BUILD_CMD)

flash: flash1 flash2
	@echo "📦 Flash done for both ESP32 !"

flash1:
	@echo "⚡ Flash ESP32 #1 on $(PORT1)"
	$(FLASH_CMD) $(PORT1)

flash2:
	@echo "⚡ Flash ESP32 #2 on $(PORT2)"
	$(FLASH_CMD) $(PORT2)

