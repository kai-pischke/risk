# Risk
This will be an implementation of the RISK Board game in Haskell. It is being written by the first year Computer Science students at Somerville College Oxford as a summer project over the summer of 2020.

## Structure 

### app/

Contains the `Main.hs` file. Not very useful right now since we haven't done much yet. In the end the `Main.hs` file will import the modules from the `src` folder. 

### src/

Contains all the seperate modules that will be part of the final program.

### test/

Contains all the test cases. Ideally each module should have a corresponding test case. The file name should be of the form `<module name>Spec.hs`, so if the test case is for the `Graph.hs` module then the test would be called `GraphSpec.hs`. There is also a file in this folder that is just called `Spec.hs` whose job it is to find all the other test files in the folder ending in `Spec`. You should be able to run all tests using `stack test`.

### interface.md

This is meant to show the interface that each module should expose. Ideally any functionality should first be specified in `interface.md` then you should write a test file (and put it in the `test/` folder) and only then should you write the module. 

### stack.yaml, package.yaml and risk.cabal
Don't edit `risk.cabal`. Anything you can do in that file you can also do in `package.yaml` which is just a fancy version of it that is used to automatically generate the `risk.cabal` file. If you delete `risk.cabal` then stack will make a new one on the next build. If you accidently edit `risk.cabal` the best thing to do is just to delete it. In general everything you need will be in `package.yaml`, but you may also need to specify the version of external dependencies in `stack.yaml`.
