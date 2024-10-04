# Urli Lab Pressure-drop flow-meter Electronic Box Controller


## Info
- E-mail: urli.morgane@uqam.ca
- Website: https://www.morganeurli.com


## Setup

The following script create and activate a python virtual environnement and install all dependencies.

```shell
python3 -m venv .venv
source .venv/bin/activate
pip install -r requirements.txt
```
## Quickstart

Note: This require PowerShell script execution permission. 

### Flashing Micropython on esp32 device

Download micropython binary from https://micropython.org/resources/firmware/ESP32_GENERIC_S3-20240105-v1.22.1.bin
Press the RESET and BOOT button simultaneously and then release RESET button. The programming serial port should now be available. (See https://docs.espressif.com/projects/esptool/en/latest/esp32/esptool/index.html) 

```shell
python -m esptool --chip auto --port YOUR_COM_PORT erase_flash
python -m esptool --chip auto --port YOUR_COM_PORT write_flash --flash_size detect -z 0 YOUR_PATH_TO_MICROPYTHON_BIN

```
Then, reboot the controller by pressing RESET only. The new COM port should be available.

### Upload Pressure-drop flow-meter source code to the ESP32 MicroPython filesystem
```shell
mpremote connect YOUR_COM_PORT fs cp src/ds18B20.py :
mpremote connect YOUR_COM_PORT fs cp src/honeywell_mpr.py :
mpremote connect YOUR_COM_PORT fs cp src/led.py :
mpremote connect YOUR_COM_PORT fs cp src/main.py :
mpremote connect YOUR_COM_PORT fs cp src/user_button.py :
```

### Enter repl session
```shell
mpremote connect YOUR_COM_PORT
```

### Leaving repl session
Press Ctrl+] keys.

### esp32 soft-reset
In repl session without micropython app code running, then press Ctrl+D keys.

### Removing all files from ESP32 MicroPython filesystem
```shell
mpremote connect YOUR_COM_PORT + exec "import os, uos; [uos.remove(f) for f in os.listdir()]"
```
