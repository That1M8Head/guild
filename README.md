# Guild

Guild ~~is~~ will be a cross-platform operating environment built in Racket. Not an operating system, or a shell, but an operating environment - like an OS, but functionally just a shell atop an existing OS. It's called "Guild" because I was originally going to use GNU Guile, but decided to use Racket at the last moment for cross-platform reasons (i.e. Guile is not on Windows).

> Guild isn't real yet. Don't expect it to be real. If you think it's real, you will be disappointed. But rest assured it will become real soon. And by soon I mean whenever I feel like it.
> I seriously just parked this repository here early...

## Features (for the future!)

- Cross-platform
  - Guild will be available for Windows, macOS and Linux at a minimum.
- Completely free/libre software
  - Guild is licensed under the GNU General Public License 3.0, so no one can
  use it for non-free stuff.
- Written entirely in Racket
  - Writing Guild entirely in Racket ensures it can be used in basically any
  Racket environment.
- Hyper-extensible with Racket (like GNU Emacs with Elisp)
  - Designing Guild for extensibility will enable users to create whatever they
  want for Guild, just like how Emacs' extensibility allows users to write
  basically anything they want for the editor.
- Suitable as a shell replacement
  - Guild will be designed with a `sh`-like interface from the beginning, making
  it suitable for use as a shell replacement.
- Suitable as a development environment
  - Guild's planned extensibility will allow users to write programming
  extensions for the environment, allowing for a wide variety of development
  workflows.
- Suitable as an API for other Racket projects
  - Guild will provide a standard library and API (Application Programming
  Interface) for other Racket projects to take advantage of the Guild
  environment.
- Suitable as a pseudo-operating system
  - One of Guild's top priorities is providing an operating system-like
  interface on top of an existing operating system, even if that system is a
  bare-bones Arch system.
- Provides abstractions for OS-specific functionality
  - Guild will provide abstractions for OS-specific functionality, such as
  filesystem access, process management, etc.

## Installation

Don't.

Seriously, don't.

It's not ready yet.

## Development Plan (not for users)

- [X] Decide to use GNU Guile
- [X] Name the project "Guild"
- [X] Realise Guile is not available for Windows
- [X] Decide to use Racket instead
- [X] Install Racket
- [X] Set up Guild's structure
- [X] Write some Racket code just to test
- [X] Write something that gives the illusion of a real Guild environment
- [X] Split off the core into separate files
- [X] Implement Lisp evaluation
- [ ] Write something resembling a standard library
- [ ] Documentation
  - [X] This README
  - [ ] Docstrings
  - [ ] Documentation files
  - [ ] User manual
- [ ] Outline an API
- [ ] Go insane trying to code the API
- [ ] Actually code the API
- [ ] Was that it?
- [ ] Come up with more things to add to the plan
