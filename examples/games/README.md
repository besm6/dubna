# Games Examples

This directory contains classic computer games implemented for the BESM-6 Dubna system. These games demonstrate interactive programming techniques and showcase the capabilities of the system for entertainment and educational purposes.

## Available Games

### Adventure Games
- **`adventure1.dub`** - Simple text adventure game in Pascal
  - Basic room navigation
  - Object interaction
  - Simple puzzle solving
  - Good introduction to adventure game programming

- **`adventure2/`** - Advanced adventure game (multi-file)
  - **`main.pas`** - Main game logic
  - **`database.txt`** - Game data and descriptions
  - **`maze.pas`** - Maze generation and navigation
  - **`u1.pas`, `u2.pas`** - Utility modules
  - **`m2.pas`** - Additional game mechanics
  - **`adv2.pas`** - Adventure game engine

- **`adventure3/`** - Complex adventure game with database
  - **`mtadvent.pas`** - Main adventure program
  - **`mtadv.txt`** - Adventure database
  - **`cmds1.pas`, `cmds2.pas`, `cmds3.pas`** - Command processing
  - **`locs.pas`** - Location management
  - **`mt1.pas`** - Game mechanics
  - **`probs.pas`** - Problem solving

### Classic Games
- **`delmar.dub`** - Delmar game
  - Strategic gameplay
  - Resource management
  - Decision making

- **`guess.dub`** - Number guessing game
  - Simple interactive game
  - Good for learning input/output
  - Demonstrates basic game loops

- **`hamurabi.dub`** - Ancient city management game
  - Resource management simulation
  - Historical context (ancient Mesopotamia)
  - Strategic decision making
  - Population and agriculture simulation

- **`oregon.dub`** - Oregon Trail-style game
  - Historical simulation
  - Resource management
  - Random events
  - Survival mechanics

- **`quinio.dub`** - Quinio game
  - Puzzle game mechanics
  - Logical thinking
  - Pattern recognition

- **`wumpus.dub`** - Hunt the Wumpus
  - Classic cave exploration game
  - Risk assessment
  - Strategic movement
  - Hazard avoidance

## Running Games

### Single-file Games
```bash
# Run simple adventure
dubna adventure1.dub

# Run guessing game
dubna guess.dub

# Run Hamurabi
dubna hamurabi.dub

# Run Oregon Trail
dubna oregon.dub

# Run Hunt the Wumpus
dubna wumpus.dub
```

### Multi-file Games
```bash
# Run advanced adventure (adventure2)
dubna adventure2/main.pas

# Run complex adventure (adventure3)
dubna adventure3/mtadvent.pas
```

## Game Features

### Interactive Elements
- **Text-based interface** - All games use text input/output
- **Menu systems** - Structured command interfaces
- **Save/load functionality** - Game state persistence
- **Random events** - Procedural content generation

### Programming Techniques Demonstrated
- **State management** - Game state tracking
- **Input validation** - User input processing
- **Data structures** - Game data organization
- **Algorithm implementation** - Game logic
- **Error handling** - Robust game operation

## Historical Context

These games represent some of the earliest interactive computer games developed in the Soviet Union. They demonstrate:

- **Educational programming** - Games as learning tools
- **System capabilities** - Showcasing BESM-6 features
- **User interface design** - Text-based interaction patterns
- **Game design principles** - Early game mechanics

### Cultural Significance
- **Soviet computing culture** - Games in academic environments
- **Educational tools** - Programming through game development
- **System demonstration** - Showcasing computer capabilities
- **Entertainment value** - Early computer entertainment

## Technical Notes

### Language Support
- **Pascal** - Primary language for adventure games
- **Assembly** - Some games use low-level programming
- **Fortran** - Mathematical games and simulations

### System Requirements
- **Memory usage** - Games designed for 32K word memory
- **I/O operations** - Console input/output
- **File operations** - Save/load functionality
- **Random number generation** - Procedural content

### Game Design Patterns
- **Command parser** - Text command processing
- **State machine** - Game state management
- **Data-driven design** - External data files
- **Modular architecture** - Multi-file organization

## Contributing

To add new games:
1. Follow the existing naming conventions
2. Include proper documentation
3. Test thoroughly on the emulator
4. Update this README with game description
5. Ensure compatibility with BESM-6 limitations

## References

- **Adventure Game Programming** - Techniques for text adventures
- **Interactive Fiction** - History of text-based games
- **Soviet Computing** - Historical context of BESM-6 games
- **Game Design Patterns** - Classic game mechanics
