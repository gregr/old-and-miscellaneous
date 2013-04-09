import font
import surface
import pygame
from pygame.locals import *

def main():
    pygame.init()
    try:
        screen = pygame.display.set_mode((640, 480))
        screen.fill((0, 64, 0))
        text = font.Text("Eggs are good for you, but not on the eiffel tower",
                         font.create("courier", 24), (0, 0, 255), (0, 0, 0),
                         True)
        text2 = font.Text("['abc', 'zebra', 123]",
                          font.create("courier", 24), (0, 0, 255), None,
                          True)
        text2._surface = surface.rotate(text2._surface, 90)
        text.draw(screen, (20, 20))
        text2.draw(screen, (20, 50))
        pygame.display.flip()
        while True:
            pygame.event.pump()
    finally:
        pygame.quit()

# if __name__ == "__main__":
#     main()
