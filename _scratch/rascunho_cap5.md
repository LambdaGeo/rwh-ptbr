

http://seanhess.github.io/2015/08/04/practical-haskell-getting-started.html
https://sakshamsharma.com/2018/03/haskell-proj-struct/

criando o projeto:

Let us now create a fancy new project, which prints different messages depending on the time of the day. Navigate to a folder where you want to create your new project (do not create the folder for the project). Run:

stack new hs2json


This would take some time, the first time around. This is because stack is downloading its own version of GHC (so that it does not conflict with your system installations).

Once it is finished, you should see a new folder of the name my-cool-project. If you enter it, you may see the following folder structure.
```
.
├── app
│   └── Main.hs
├── ChangeLog.md
├── hs2json.cabal
├── LICENSE
├── package.yaml
├── README.md
├── Setup.hs
├── src
│   └── Lib.hs
├── stack.yaml
└── test
    └── Spec.hs
```

Haskell projects conventionally have project names comprising of one word, or two/three small words separated by hyphens.

File description
Let’s talk about these files one by one:

README.md: The main readme of your project. Make sure you put a nice description of your project in this file.

stack.yaml: Configuration for stack. Contains instructions about where to download dependencies from, what sort of versions to select etc.

package.yaml: Your project’s build configuration. It lists dependencies of your project, the binaries to build, the libraries to link, runtime commands, build commands etc. More on this in the section on dependencies. You should definitely edit this file’s initial few lines, since they describe your project, and you (its maintainer) in more detail.

my-cool-project.cabal: File generated from package.yaml by stack, for backwards compatibility. DO NOT EDIT THIS MANUALLY. Stack will ignore package.yaml and use this file, if you edit it manually. It is safe to delete this file, stack will regenerate it automatically.

LICENSE: The license your project is available under. Remember to write your name inside thie file.

ChangeLog.md: An optional changelog that you can maintain for your project, to help users during version upgrades.

Setup.hs: This file can be ignored for now. It allows for very complex build configurations, although is not required in almost all cases.

app folder: This folder contains your main function, among other code from your project that will not be useful for people who may import your project as a library. This folder often contains a single file, which launches some function placed inside src, although you are free to put any Haskell code here and use it as it is.

src folder: This folder often contains the major code of a project. See src/Lib.hs for an example file. The main function is present in app/Main.hs, which calls the someFunc function in src/Lib.hs.

test folder: This folder contains tests for your code. More on this in the section on writing tests.

Running your test project
Before we do this, you should look at the files app/Main.hs and src/Lib.hs, if you haven’t already done so.

Also note, stack does not use anything from your system installation of haskell/ghc/cabal. Thus, we will build and run our program in a slightly different fashion than before.

cd hs2json
stack build
stack run

Ira imprimir

someFunc

Note that my-cool-project-exe is the name of your project’s executable, which stack will run in the correct environment, where all the configured dependencies etc are available. You can modify some of the code (go do it!), run stack build and then stack exec my-cool-project-exe to re-run it.

Quick note: If your project requires CLI arguments to run, you can pass them in this manner: stack exec my-cool-project-exe -- arg1 arg2 arg3 .... The arguments before -- are interpreted by stack, and the ones after it are passed directly to your binary’s environment.

Structuring your code
Now that the basics are out of the way, let us look at how to structure Haskell project source code.

Modules
You write your code in files whose names start with capital letters. Each of these files is called a module. If you have a file src/MyFile.hs, it must look (in the simplest case) like:

module MyFile where

import Data.Text
import qualified Control.Monad as M

fancyFxn :: String -> Bool
fancyFxn _ = False

Note that the file name and the module name match exactly, except for the .hs. If they do not match, your code won’t build.

Modules are a neat way to organize your code. You can keep related functionality in a single module, which makes it easy to search for functionality around your code.

