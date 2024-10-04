from machine import SoftI2C
import time
from micropython import const
import uasyncio as asyncio
import math

# Default I2C address for the MPR sensor
_MPR_DEFAULT_ADDR = const(0x18)


class TransferFunction:
    # Maximum count value for the sensor
    max_count = const(16777215)

    def __init__(self, min, max):
        self.min = min  # Minimum raw value
        self.max = max  # Maximum raw value
        self.range = max - min  # Range of raw values

    def apply(self, raw_value: int):
        # Apply the transfer function to convert raw value to a relative value
        return (raw_value - self.min) / self.range


# Predefined transfer functions for different sensors
transfer_function_a = TransferFunction(1677721, 15099495)
transfer_function_b = TransferFunction(4194, 3774874)
transfer_function_c = TransferFunction(3355443, 13421773)


class MprSensor:
    def __init__(
        self,
        i2c: SoftI2C,
        i2c_address: int = _MPR_DEFAULT_ADDR,
        transfer_function: TransferFunction = transfer_function_a,
        pressure_range: tuple = (0.0, 1.0),
        label: str = "",
    ):

        self.i2c = i2c
        self.i2c_address = i2c_address
        self.label = label
        self.pressure_range = pressure_range
        self.transfer_function = transfer_function

    def _format_exception(self, msg):
        return f"MPR sensor {self.label} at address {self.i2c_address}: {msg}"

    async def get_measurement(self, oversampling_rate: int = 1):
        i = 0
        sum_pres = 0.0
        while True:
            try:
                p = await self.read_data()
            except OSError:
                return "X"
            sum_pres += p
            i += 1
            if i == oversampling_rate:
                sum_pres /= oversampling_rate  # Average the measurements
                break
        return sum_pres

    async def read_data(self):
        # Read raw data from the sensor
        self.i2c.writevto(
            self.i2c_address, [b"\xAA", b"\x00", b"\x00"]
        )  # Send command to the sensor
        retry_count = 0
        while True:
            await asyncio.sleep_ms(5)  # Wait for the sensor to process the command
            data = self.i2c.readfrom(
                self.i2c_address, 4
            )  # Read 4 bytes of data from the sensor
            powered = bool(data[0] & (1 << 6))  # Check if the sensor is powered
            busy = bool(data[0] & (1 << 5))  # Check if the sensor is busy
            mem_error = bool(data[0] & (1 << 2))  # Check for memory error
            math_sat = bool(data[0] & 1)  # Check for math saturation error
            retry_count += 1
            if mem_error or not powered:
                raise Exception(
                    self._format_exception(
                        f"Error status(powered={powered}, busy={busy}, mem_error={mem_error}, math_sat={math_sat} data={data})"
                    )
                )
            if retry_count > 5:
                return math.nan
            if math_sat:
                return math.inf
            if not busy:
                raw = (
                    (data[1] << 16) | (data[2] << 8) | data[3]
                )  # Combine the raw data bytes
                relative_value = self.transfer_function.apply(
                    raw
                )  # Apply the transfer function
                converted_pressure = (
                    relative_value * (self.pressure_range[1] - self.pressure_range[0])
                    + self.pressure_range[0]
                )  # Convert to pressure
                return converted_pressure
