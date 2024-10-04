import sys
import time
from machine import SoftI2C, Pin

import machine
machine.freq(240000000)

import led
import uasyncio as asyncio

# Initialize the LED status indicator
status = led.LED(Pin(35), 100, led.Colors.YELLOW)

# Initialize global variables
step = 0
measure_lock = asyncio.Lock()
timer_offset = time.ticks_ms()
measurement_period_ms = 2000
measuring = False

try:
    # Import the Honeywell MPR sensor library
    import honeywell_mpr

    # Initialize I2C interfaces for pressure sensors
    i2c_pressure_1 = SoftI2C(scl=Pin(44), sda=Pin(43), freq=100000)
    i2c_pressure_2 = SoftI2C(scl=Pin(2), sda=Pin(1), freq=100000)

    pressure_sensor_1 = honeywell_mpr.MprSensor(i2c_pressure_1, 
                                                transfer_function=honeywell_mpr.transfer_function_c, 
                                                pressure_range=(0.0,1.0),
                                                label="pressure_1")
    pressure_sensor_2 = honeywell_mpr.MprSensor(i2c_pressure_2, 
                                                transfer_function=honeywell_mpr.transfer_function_c, 
                                                pressure_range=(0.0,1.0),
                                                label="pressure_2")
    
    from ds18B20 import DS18B20

    temperature_sensor_1 = DS18B20(data_pin=Pin(42))

    # Import and initialize the user button
    from user_button import UserButton

    button = UserButton(Pin(4))

    # Coroutine to handle long press events
    async def on_long_press_event():
        global step, timer_offset, measure_lock, measuring
        while True:
            await button.long_press_event.wait()
            status.set_color(led.Colors.MAGENTA)
            step = 0
            if measuring:
                await measure_lock.acquire()
                measuring = False
            await asyncio.sleep(1)
            status.set_color(led.Colors.GREEN)

    # Coroutine to handle short press events
    async def on_short_press_event():
        global step, measure_lock, measuring, timer_offset
        while True:
            await button.short_press_event.wait()
            if measuring:
                await measure_lock.acquire()
                status.set_color(led.Colors.GREEN)
            else:
                step += 1
                if step == 1:
                    timer_offset = time.ticks_ms()
                measure_lock.release()
                status.set_color(led.Colors.BLUE)
            measuring = not measuring

    # Main coroutine
    async def main():
        await measure_lock.acquire()

        # Create tasks for button press events
        asyncio.create_task(on_long_press_event())
        asyncio.create_task(on_short_press_event())

        status.set_color(led.Colors.GREEN)

        while True:
            async with measure_lock:
                t = time.ticks_ms()
                # Gather measurements from sensors
                values = await asyncio.gather(
                    pressure_sensor_1.get_measurement(100),
                    pressure_sensor_2.get_measurement(100),
                    temperature_sensor_1.get_measurement(),
                )
                pressure_1 = values[0]
                pressure_2 = values[1]
                temperature_3 = values[2]

                tab_separed_values = '\t'.join([str(value) for value in [time.ticks_diff(time.ticks_ms(),timer_offset)//1000,
                                                                        step,
                                                                        pressure_1,
                                                                        pressure_2,
                                                                        temperature_3]])
                print(tab_separed_values)

                # Sleep for the remaining measurement period
                await asyncio.sleep_ms(
                    max(0, measurement_period_ms - time.ticks_diff(time.ticks_ms(), t))
                )

    # Run the main coroutine
    asyncio.run(main())

except Exception as e:
    # Handle exceptions and set LED to red
    sys.print_exception(e)
    status.set_color(led.Colors.RED)
