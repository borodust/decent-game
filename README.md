# DECENT GAME


## Install and run
Clone or link repository into `~/quicklisp/local-projects/`

```lisp
(ql-dist:install-dist "http://bodge.borodust.org/dist/org.borodust.bodge.testing.txt")
(ql:update-all-dists)

(ql:quickload :decent-game)
(decent-game:play)
```

## Debug

Once game started invoke a form below. To reload a scene - invoke the form again.

### Dialogue
```lisp
(gamekit.fistmachine:transition-to 'decent-game::dialogue-debug-screen)
```

### Animation
```lisp
(gamekit.fistmachine:transition-to 'decent-game::init-animation-debug-screen)
```

### Gameplay
```lisp
(gamekit.fistmachine:transition-to 'decent-game::init-gameplay-debug-screen)
```
