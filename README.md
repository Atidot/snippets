# snippets
### Atidot Code Snippets  
* Single file
* Runnable
* Reproducible
* 0 setup (no hard build / install)
* Using Nix-"shebang" line (https://nixos.org/nix/manual/#use-as-a-interpreter)
  
These are useful as a starting point for playing around and experimenting with Haskell packages, without caring too much about build environment or tools (Cabal, Stack, Nix, etc.).  
#### Disclaimer
We DO NOT encourage this hack-ish ugly one-liner style for production code.  
For production we recommend using organized Nix source trees with as much https://dhall-lang.org/ as possible.
## Credits
Credit goes to these wonderful packages and their creators:  
~~~shell
~/atidot/snippets/Haskell (master) $ grep import *.hs | grep -o '".*"' | sort | uniq | sed -E 's|"(.*)"|http://hackage.haskell.org/package/\1|g'
http://hackage.haskell.org/package/base
http://hackage.haskell.org/package/bytestring
http://hackage.haskell.org/package/casing
http://hackage.haskell.org/package/conduit
http://hackage.haskell.org/package/conduit-extra
http://hackage.haskell.org/package/directory
http://hackage.haskell.org/package/http-types
http://hackage.haskell.org/package/inline-java
http://hackage.haskell.org/package/jvm
http://hackage.haskell.org/package/lens
http://hackage.haskell.org/package/lucid
http://hackage.haskell.org/package/parallel
http://hackage.haskell.org/package/pyfi
http://hackage.haskell.org/package/reflex
http://hackage.haskell.org/package/reflex-dom
http://hackage.haskell.org/package/resourcet
http://hackage.haskell.org/package/retry
http://hackage.haskell.org/package/sbv
http://hackage.haskell.org/package/shakespeare
http://hackage.haskell.org/package/tagsoup
http://hackage.haskell.org/package/template-haskell
http://hackage.haskell.org/package/text
http://hackage.haskell.org/package/time
http://hackage.haskell.org/package/unagi-bloomfilter
http://hackage.haskell.org/package/wai
http://hackage.haskell.org/package/wai-extra
http://hackage.haskell.org/package/warp
http://hackage.haskell.org/package/warp-tls
http://hackage.haskell.org/package/wreq
~~~
## AlternativeTest.hs
~~~shell
~/atidot/snippets/Haskell (master) $ ./AlternativeTest.hs
Nothing
[2,4,6,4,6,4,6,8,6,8,6,8,10]
Just 11
"shouldn't get here"
AlternativeTest.hs: user error (mzero)
~~~
## AzureCLICodegen.hs
Generating Haskell data types for Azure CLI commands based on online documentation.  
For example: https://docs.microsoft.com/en-us/cli/azure/group?view=azure-cli-latest#az-group-list  
~~~shell
~/atidot/snippets/Haskell (master) $ ./AzureCLICodegen.hs
data Group
    = Create {_groupCreate_location :: !String,
              _groupCreate_name :: !String,
              _groupCreate_subscription :: !(Maybe String),
              _groupCreate_tags :: !(Maybe String)}
    | Delete {_groupDelete_name :: !String,
              _groupDelete_noWait :: !(Maybe String),
              _groupDelete_subscription :: !(Maybe String),
              _groupDelete_yes :: !(Maybe String)}
    | Exists {_groupExists_name :: !String,
              _groupExists_subscription :: !(Maybe String)}
    | Export {_groupExport_name :: !String,
              _groupExport_includeComments :: !(Maybe String),
              _groupExport_includeParameterDefaultValue :: !(Maybe String),
              _groupExport_subscription :: !(Maybe String)}
    | List {_groupList_subscription :: !(Maybe String),
            _groupList_tag :: !(Maybe String)}
    | Show {_groupShow_name :: !String,
            _groupShow_subscription :: !(Maybe String)}
    | Update {_groupUpdate_name :: !String,
              _groupUpdate_add :: !(Maybe String),
              _groupUpdate_forceString :: !(Maybe String),
              _groupUpdate_remove :: !(Maybe String),
              _groupUpdate_set :: !(Maybe String),
              _groupUpdate_subscription :: !(Maybe String),
              _groupUpdate_tags :: !(Maybe String)}
    | Wait {_groupWait_name :: !String,
            _groupWait_created :: !(Maybe String),
            _groupWait_custom :: !(Maybe String),
            _groupWait_deleted :: !(Maybe String),
            _groupWait_exists :: !(Maybe String),
            _groupWait_interval :: !(Maybe String),
            _groupWait_subscription :: !(Maybe String),
            _groupWait_timeout :: !(Maybe String),
            _groupWait_updated :: !(Maybe String)}
    deriving (Show, Read, Eq)
~~~
## Conduits.hs
~~~shell
~/atidot/snippets/Haskell (master) $ echo -e "hello\nworld" | ./Conduits.hs
HELLO
WORLD
~~~
## ForceSSL.hs
~~~shell
root@berkos:/home/barak/Development/atidot/snippets/Haskell# ./ForceSSL.hs
~~~
## Java.hs
~~~shell
~/atidot/snippets/Haskell (master) $ ./Java.hs
[1 of 1] Compiling Main             ( Java.hs, Java.o ) [flags changed]
Linking Java ...
~/atidot/snippets/Haskell (master) $ ./Java
~~~
## Pyfi.hs
~~~shell
~/atidot/snippets/Haskell (master) $ ./Pyfi.hs
------------
From Python:
   0  1  2
0  1  2  3
1  4  5  6
2  7  8  9
"-------------"
"From Haskell:"
[12,15,18]
~~~
## Retry.hs
~~~shell
~/atidot/snippets/Haskell (master) $ ./Retry.hs
"Still waiting for file..."
"Still waiting for file..."
"Still waiting for file..."
"Still waiting for file..."
"Still waiting for file..."
"Still waiting for file..."
~~~
## SBV.hs
~~~shell
~/atidot/snippets/Haskell (master) $ ./SBV.hs
Satisfiable. Model:
  x =  4 :: Integer
  y = -3 :: Integer
~~~
## Strategies.hs
~~~shell
~/atidot/snippets/Haskell (master) $ ./Strategies.hs
[1 of 1] Compiling Main             ( Strategies.hs, Strategies.js_o )
Linking Strategies.jsexe (Main)
~/atidot/snippets/Haskell (master) $ node Strategies.jsexe/all.js
warning, unhandled primop: SparkOp (1,1)
uncaught exception in Haskell main thread: ReferenceError: h$primop_SparkOp is not defined
ReferenceError: h$primop_SparkOp is not defined
    at h$$wV (/home/barak/Development/atidot/snippets/Haskell/Strategies.jsexe/all.js:46753:3)
    at h$runThreadSlice (/home/barak/Development/atidot/snippets/Haskell/Strategies.jsexe/all.js:8625:11)
    at h$runThreadSliceCatch (/home/barak/Development/atidot/snippets/Haskell/Strategies.jsexe/all.js:8577:12)
    at Immediate.h$mainLoop [as _callback] (/home/barak/Development/atidot/snippets/Haskell/Strategies.jsexe/all.js:8572:9)
    at runCallback (timers.js:781:20)
    at tryOnImmediate (timers.js:743:5)
    at processImmediate [as _immediateCallback] (timers.js:714:5)

~~~
## TestReflex.hs
~~~shell
~/atidot/snippets/Haskell (master) $ ./TestReflex.hs
Linking TestReflex.jsexe (Main)
~/atidot/snippets/Haskell (master) $ firefox TestReflex.jsexe/index.html
~~~
## UnagiBloom.hs
~~~shell
~/atidot/snippets/Haskell (master) $ ./UnagiBloom.hs
Linking UnagiBloom.jsexe (Main)
~/atidot/snippets/Haskell (master) $ node UnagiBloom.jsexe/all.js
True
True
~~~
## WebBlog.hs
~~~shell
~/atidot/snippets/Haskell (master) $ ./WebBlog.hs
<html><head><title>The Title</title><meta charset="utf-8"><meta content="width=device-width, initial-scale=1" name="viewport"><script src="https://cdnjs.cloudflare.com/ajax/libs/d3/5.9.1/d3.min.js"></script><link href="https://cdnjs.cloudflare.com/ajax/libs/c3/0.6.12/c3.min.css" rel="stylesheet"><script src="https://cdnjs.cloudflare.com/ajax/libs/c3/0.6.12/c3.min.js"></script></head><body><h1>Intorduction</h1><p>lorem ipsumlorem ipsumlorem ipsumlorem ipsum</p><h2>Part 1</h2><p>lorem ipsumlorem ipsumlorem ipsum<br><br>lorem ipsum<ul><li><a href="http://www.wikipedia.org">A</a></li><li><a href="http://www.wikipedia.org">B</a></li><li><a href="http://www.wikipedia.org">C</a></li></ul></p><h2>Part 2</h2><p>lorem ipsumlorem ipsumlorem ipsum<b>lorem ipsum</b>lorem ipsum<br>lorem ipsum<b>lorem ipsum</b>lorem ipsum<ul><li><a href="http://www.wikipedia.org">X</a></li><li><a href="http://www.wikipedia.org">Y</a></li><li><a href="http://www.wikipedia.org">Z</a></li></ul></p><div id="chart"></div><script>
        var chart = c3.generate({
            bindto: "#chart",
            grid: {
                x: {
                    show: true
                },
                y: {
                    show: true
                }
            },
            axis: {
              y : {
                tick: {
                  format: d3.format("$,")
                }
              }
            },
            data: {
                type: "bar",
                x: "x",
                json: {
                    x: [2010, 2011, 2012, 2013, 2014, 2015],
                    A: [30, 20, 50, 40, 60, 50],
                    B: [200, 130, 90, 240, 130, 220],
                    C: [300, 200, 160, 400, 250, 250]
                }
            }
        });
    </script><h2>Conslusion</h2><p>lorem ipsumlorem ipsum<br>lorem ipsum<br></p></body></html>
~/atidot/snippets/Haskell (master) $ ./WebBlog.hs > WebBlog.html
~/atidot/snippets/Haskell (master) $ firefox WebBlog.html
~~~
