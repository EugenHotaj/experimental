import pygame

screen = pygame.display.set_mode((600, 400))
running = True

while running:
  pygame.event.poll()

  if event.type == pygame.QUIT:
    running = False
