from neopixel import NeoPixel
from machine import Pin 

# Define a class to hold color constants
class Colors:
    OFF     = (0,0,0)  # LED off
    WHITE   = (255,255,255)  # White color
    RED     = (255,0,0)  # Red color
    GREEN   = (0,255,0)  # Green color
    BLUE    = (0,0,255)  # Blue color
    YELLOW  = (255,255,0)  # Yellow color
    CYAN    = (0,255,255)  # Cyan color
    MAGENTA = (255,0,255)  # Magenta color
    ORANGE  = (255,128,0)  # Orange color

# Define a class to control the LED
class LED:
    def __init__(self, pin: Pin, intensity: int=100, color: tuple=Colors.OFF):
        self._np = NeoPixel(pin, 1)  # Initialize NeoPixel with the given pin and 1 LED
        self.color = color  # Set initial color
        self.intensity = intensity  # Set initial intensity
        self.set_intensity(intensity, True)  # Update LED with initial intensity and color
        
    def set_color(self, color):
        # Set the color of the LED with the current intensity
        self._np[0] = [int(rgb * self.intensity / 100.0) for rgb in color]
        self._np.write()  # Write the color to the LED
        self.color = color  # Update the current color
        
    def set_intensity(self, intensity, update_led=False):
        # Set the intensity of the LED, ensuring it is between 0 and 100
        self.intensity = max(min(intensity, 100), 0)
        if update_led:
            self.set_color(self.color)  # Update the LED color with the new intensity if requested