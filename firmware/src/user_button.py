from machine import Pin
import time
import uasyncio as asyncio


class UserButton:

    def __init__(self, pin: Pin):
        # Initialize the UserButton with a specific pin
        self.pin = pin
        # Set the pin as an input with no pull-up or pull-down resistor
        self.pin.init(Pin.IN, pull=None)
        # Set up an interrupt on the pin for both rising and falling edges
        self.pin.irq(
            trigger=Pin.IRQ_FALLING | Pin.IRQ_RISING, handler=self._pin_callback
        )
        # Record the current time in milliseconds
        self._timestamp = time.ticks_ms()
        # Store the last triggered state of the pin
        self._last_trigged_state = self.pin.value()
        # Initialize flags for short and long press events
        self.short_press_event = asyncio.ThreadSafeFlag()
        self.long_press_event = asyncio.ThreadSafeFlag()

    def _pin_callback(self, pin):
        # Callback function to handle pin interrupts
        state = pin.value()  # Get the current state of the pin
        if state and not self._last_trigged_state:
            # If the pin state has changed from low to high
            delta = time.ticks_diff(
                time.ticks_ms(), self._timestamp
            )  # Calculate the time difference
            if delta < 100:
                # Ignore if the press duration is less than 100 ms (debounce)
                pass
            elif delta < 1000:
                # If the press duration is between 100 ms and 1 second, it's a short press
                self.short_press_event.set()
            else:
                # If the press duration is more than 1 second, it's a long press
                self.long_press_event.set()
        else:
            # If the pin state has changed from high to low, update the timestamp
            self._timestamp = time.ticks_ms()
        # Update the last triggered state
        self._last_trigged_state = state
