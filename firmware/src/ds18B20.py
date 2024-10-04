from machine import Pin
import onewire
import ds18x20
import uasyncio as asyncio

class DS18B20:
    def __init__(self, data_pin: Pin):
        # Initialize the OneWire bus on the specified data pin
        self.bus = ds18x20.DS18X20(onewire.OneWire(data_pin))

        # Scan for DS18X20 devices on the bus
        _ids = self.bus.scan()

        # Raise an exception if no devices are found
        if len(_ids) == 0:
            raise Exception("No DS18X20 devices found")

        # Raise an exception if more than one device is found
        if len(_ids) > 1:
            raise Exception("More than one DS18X20 device found")

        # Store the ID of the found device
        self._id = _ids[0]

    async def get_measurement(self, oversampling_rate: int = 1):
        # Initialize variables for oversampling
        i = 0
        sum_temp = 0.0

        # Loop to perform temperature measurements
        while True:
            # Start temperature conversion
            self.bus.convert_temp()

            # Wait for the conversion to complete (750 ms)
            await asyncio.sleep_ms(750)

            # Read the temperature and add to the sum
            sum_temp += self.bus.read_temp(self._id)
            i += 1

            # Check if the desired number of samples is reached
            if i == oversampling_rate:
                # Calculate the average temperature
                sum_temp /= oversampling_rate
                break

        # Return the average temperature
        return sum_temp
