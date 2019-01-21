
[Sumário](index)

## Capítulo 11. Testes e garantia de qualidade         


Construir sistemas reais significa ter cuidado com o controle de qualidade, robustez e corretude. Com os mecanismos certos para a garantia de qualidade, código bem-escrito pode parecer uma máquina precisa, com todas as funções executando suas tarefas de acordo com as especificações. Não há desleixo nas situações críticas o que resulta em código que é auto-explicativo – e obviamente correto – do tipo que inspira confiança.

Em Haskell, existem diversas ferramentas à disposição para construir tais sistemas precisos. A ferramenta mais óbvia, e construída na própria linguagem é o sistema de tipos expressivo, garante que toda verificação seja feita estaticamente – tornando impossível escrever código que viole tais restrições. Adicionalmente, pureza e polimorfismo promovem um estilo de código que é modular, refatorável e testável. Este é o tipo de código que não contém erros.

Os testes possuem um papel importante para manter o código no caminho certo. Os principal mecanismo de teste em Haskell são os tradicionais teste de unidade (por meio da biblioteca HUnit) e o seu descendente mais poderoso, teste baseado em propriedades através do QuickCheck, um framework de testes de código-livre para Haskell. Testes baseados em propriedades promovem uma abordagem de alto-nível para os testes na forma de funções invariantes que devem satisfazer universalmente, com os dados reais de testes gerados pela biblioteca para o progamador. Desta forma, o código pode ser exaustivamente testado com milhares de testes que iriam ser inviáveis para escrever manualmente, geralmente não cobrindo casos especiais que não seriam encontrados de outra forma.

Neste capítulo iremos ver como usar QuickCheck para estabelecer invariantes no código, e então re-examinar o "pretty printer" desenvolvido nos capítulos anteriores, testando-o com o framework. Iremos também ver como conduzir o processo de testes com a ferramenta de cobertura de testes do GHC: HPC.

### QuickCheck: Teste baseado em tipos

Para obter uma ideia geral sobre como funcionam os testes baseado em tipos, iremos começar com um cenário simples: você escreveu uma função específica de ordenação e deseja testar o seu comportamento.

Primeiramente, nos importamos a biblioteca QuickCheck e os módulos necessários:


```haskell
\-- file: ch11/QC-basics.hs
import Test.QuickCheck
import Data.List
```

Caso esteja usando o stack, lembre-se de adicionar o framework na lista de dependencias:

```yaml
dependencies:
- base >= 4.7 && < 5
- QuickCheck
```
E a função que nós queremos testar – uma rotina personalizada de ordenação:: 

```haskell
-- file: ch11/QC-basics.hs
qsort :: Ord a => [a] -> [a]
qsort []     = []
qsort (x:xs) = qsort lhs ++ [x] ++ qsort rhs
    where lhs = filter  (< x) xs
          rhs = filter (>= x) xs
```

Esta é a clássica implementação de ordenação em Haskell: um estudo sobre a elegância em programação funcional, não em eficiência (este não é um algoritmo de ordenação in-place, que altera a estrutura). Agora, nós queremos checar se esta função obedece às regras básicas que uma boa ordenação deveria seguir. Uma invariante útil para começar e uma que aparece com frequência em códigos puramente funcionais, é a idempotência – uma função aplicada duas vezes deve ter o mesmo resultado quando aplicada apenas uma vez. Para a nossa rotina de ordenação – um algoritmo estável de ordenação – isso deve ser sempre verdadeiro, ou a situação irá ficar feia. A invariante pode ser codificada como uma simples propriedade, da seguinte maneira

```haskell
\-- file: ch11/QC-basics.hs
prop_idempotent xs = qsort (qsort xs) == qsort xs
```

Iremos usar a conveção de QuickCheck de prefixar as propriedades de teste com `prop_` para diferenciá-las de código normal. A propriedade de idempotência é escrita simplesmente como uma função Haskell declarando uma igualdade que deve valer para todos os dados da entrada que é ordenada. Podemos checar manualmente se isso faz sentido para alguns casos simples:

```
ghci> prop_idempotent []       
True
ghci> prop_idempotent [1,1,1,1]  
True
ghci> prop_idempotent [1..100]
True
ghci> prop_idempotent [1,5,2,1,2,0,9]
True
```
Parece estar certo. Entretanto, escrever os dados de entrada à mão é tedioso e viola o código moral dos programadores funcionais eficientes: deixe a máquina fazer o trabalho! Para automatizar isto, a biblioteca QuickCheck provê um conjunto de geradores de dados para todos os tipos de dados básicos do Haskell. QuickCheck usa o `typeclass` Arbitrary para apresentar uma interface uniforme a um pseudo aleatório gerador de dados com o tipo do sistema usado para resolver a questão de qual gerador usar. QuickCheck normalmente esconde o funcionamento da geração de dados, entretanto, nós podemos também executar os geradores à mão para obter uma ideia dos dados que o QuickCheck produz. Por exemplo, gerar uma lista aleatória de valores booleanos:

```
ghci> generate  arbitrary :: IO [Bool]
[False,True,True,True,False,True,False,True]
```


QuickCheck gera dados de teste desta maneira e os passa à propriedade de nossa escolha, por meio da função quickCheck. O tipo da propriedade em si determina qual gerador de dados é usado. O `quickCheck` então checa para todos os dados de teste produzido, que a propriedade foi satisfeita. Agora, uma vez que nosso teste de idempotência é polimórfico na lista de tipos de elementos, precisamos escolher um tipo particular para o qual desejamos gerar os dados de teste, o qual iremos escrever como uma restrição de tipo da propriedade. Para executar o teste, apenas chamando quickCheck com a nossa função de propriedade, que está configurada para o tipo de dado requerido (caso contrário, o tipo de elemento da lista irá ser o padrão desinteressante tipo ())

```
ghci> (prop_idempotent :: [Integer] -> Bool)

+++ OK, passed 100 tests.
```
Para as diferentes 100 listas geradas, a nossa propriedade foi um sucesso. Quando escrever testes, geralmente é útil olhar os dados reais gerados para cada teste. Para fazer isso, iremos substituir quickCheck pelo seu irmão, verboseCheck, para ver a saída de cada teste. Por exemplo, mostrando apenas parte da saída:

```
ghci> verboseCheck (prop_idempotent :: [Integer] -> Bool)
Passed:  
[]

Passed: 
[-1]

Passed:  
[]

Passed:  
[-2,-1]

Passed:  
[2,3,-2]

...
```
Observe que os testes são aplicados a listas de diferentes tamanhos. Agora, vamos olhar para propriedades sofisticadas que a nossa função deve satisfazer.

#### Testes de propriedade

Boas bibliotecas consistem de um conjunto de primitivas ortogonais que possuem relações sensíveis entre si. Podemos usar QuickCheck para especificar as relações entre funções no nosso código, o que nos ajuda a encontrar uma boa interface para a biblioteca por meio do desenvolvimento de funções que são interrelacionadas através de propriedades úteis. QuickCheck atua desta maneira como uma ferramenta “lint” d – ela provê suporte da máquina para assegurar que a nossa biblioteca esta consistente.

A função de ordenação de lista deve certamente conter um número de propriedades interessantes que se relacionam com outras operações de lista. Por exemplo, o primeiro elemento em uma lista ordenada deve sempre ser o menor elemento da lista de entrada. Ficamos tentados a especificar essa intuição em Haskell, usando a função `minimum` da biblioteca `List:

```haskell
-- file: ch11/QC-basics.hs
prop_minimum xs         = head (qsort xs) == minimum xs
```
Testando isso, no entanto, revela um erro:
```
ghci> quickCheck (prop_minimum :: [Integer] -> Bool)
0** Exception: Prelude.head: empty list
```

A propriedade falhou quando ordenou uma lista vazia, para a qual head e minimum não estão definidas, como podemos ver pela sua definição:

```haskell
-- file: ch11/minimum.hs
head :: [a] -> a
head (x:_) = x
head [] = error "Prelude.head: empty list"

minimum :: (Ord a) => [a] -> a
minimum [] = error "Prelude.minimum: empty list"
minimum xs = foldl1 min xs
```
Portanto esta propriedade irá funcionar apenas para listas não-vazias. QuickCheck, felizmente, vem com uma linguagem própria para escrever propriedades, para que possamos especificar mais precisamente nossas invariantes, removendo valores que não queremos considerar. Para o caso da lista vazia, nós realmente queremos dizer que se a lista não está vazia, então o primeiro elemento da lista ordenada é o menor da lista de entrada. Isto é feito utilizando a função de implicação(`==>`), que remove dados inválidos antes de executar as propriedades:

```haskell
\-- file: ch11/QC-basics.hs
prop\_minimum' xs         = not (null xs) ==> head (qsort xs) == minimum xs
```
O resultado é claro. Removendo o caso da lista vazia, podemos confirmar que a propriedade de fato funciona:

```
ghci> quickCheck (prop_minimum' :: [Integer] -> Property)
00, passed 100 tests.
```
Note que tivemos que mudar o tipo da propriedade, anteriormente sendo um simples resultado Bool para agora ser um resultado mais geral do tipo Property(a propriedade em si agora é uma função que remove listas vazias, antes de testá-las, ao invés de uma simples constante booleana).

Podemos agora completar o conjunto básico de propriedades para a função de ordenação com outras invariantes que ela deve satisfazer: a saída deve ser ordenada (cada elemento deve ser menor, ou igual, ao seu sucessor); a saída deve ser uma permutação da entrada (a qual nós alcançamos através da função diferença de lista, `(\\)`); o último elemento ordenado deve ser o maior elemento; e se encontramos o menor elemento de duas listas, ele deve ser o primeiro elemento se juntarmos e ordenarmos tais listas. Estas propriedades podem ser definidas como:

```haskell
-- file: ch11/QC-basics.hs
prop_ordered xs = ordered (qsort xs)
    where ordered []       = True
          ordered [x]      = True
          ordered (x:y:xs) = x <= y && ordered (y:xs)

prop_permutation xs = permutation xs (qsort xs)
    where permutation xs ys = null (xs \\ ys) && null (ys \\ xs)

prop_maximum xs         =
    not (null xs) ==>
        last (qsort xs) == maximum xs

prop_append xs ys       =
    not (null xs) ==>
    not (null ys) ==>
        head (qsort (xs ++ ys)) == min (minimum xs) (minimum ys)
```

#### Testando sobre um modelo

Outra técnica para adquirir confiança no código é testar sobre uma implementação modelo. Podemos relacionar a nossa implementação de ordenação de lista com a função de ordenação presente na biblioteca padrão, se elas possuem o mesmo comportamento, nós ganhamos confiança que nossa função de ordenação faz o que é certo:

```haskell
-- file: ch11/QC-basics.hs
prop_sort_model xs      = sort xs == qsort xs
```

Este tipo de teste baseado em modelo é extremamente poderoso. Geralmente, desenvolvedores irão ter uma implementação de referência ou protótipo que, embora ineficiente, é correta. Isso pode então ser mantido por perto e usado para assegurar que o código de produção otimizado está de acordo com a referência. Ao construir uma grande suíte desses testes baseados em modelos e executando-os regularmente(em cada commit, por exemplo), podemos facilmente assegurar a precisão de nosso código. Grandes projetos Haskell geralmente possuem suítes de propriedades de tamanho comparável com o próprio projeto, com milhares de invariantes testadas em cada mudança, mantendo o código de acordo com a especificação e assegurando que ele se comporta como requerido.

### Caso de estudo de teste: especificando uma `pretty printer`

Testar as propriedades naturais de  funções individuais é um das mais básicas abordagens que guiam o desenvolvimento de grandes sistemas em Haskell. Veremos agora um cenário mais complicado: construir uma suíte de testes para a biblioteca de pretty-printing* desenvolvida em capítulos anteriores.

*N.dT.: Pretty-printing é o nome que se dá à apresentação de um conteúdo de maneira em que a estrutura da apresentação intensifica o sentido do próprio conteúdo

### Generating test data

Lembre-se que a pretty printer é construída de acordo com o Doc, um tipo de dado algébrico que representa documentos bem-estruturados.

```haskell
\-- file: ch11/Prettify2.hs
data Doc = Empty
         | Char Char
         | Text String
         | Line
         | Concat Doc Doc
         | Union Doc Doc
         deriving (Show,Eq)
```


A biblioteca em si é implementada como um conjunto de funções que criam e transformam valores deste tipo de documento, antes de finalmente criar a sua representação completa em uma string.

QuickCheck encoraja uma abordagem para testes onde o desenvolvedor especifica invariantes que deveriam ser verdadeiras para quaisquer dados que sejam consumidos pelo código. Para testar a biblioteca de pretty-printing, então, precisaremos de uma fonte de dados de entrada. Para isso, usufruímos da pequena suíte de combinação para construção de dados randômicos que o QuickCheck provê, via a classe Arbitrary. Essa classe fornece uma função, arbitrary, que gera dados de diferentes tipos. Com ela, podemos definir nosso gerador de dados para nossos próprios tipos de dados:

```haskell
\-- file: Test.QuickCheck
class Arbitrary a where
  arbitrary   :: Gen a
```
Algo a ser notado é geradores são executados em um ambiente Gen, indicado pelo tipo. Isso é um simples monad `state-passing` que é usada para esconder o estado do gerador de número randômico, que esta espalhado pelo código. Examinaremos monads minuciosamente em capítulos posteriores, por agora é suficientes dizer que, como Gen é definido como um `monad`, nós podemos usar sua sintaxe do para escrever novos geradores que acessam o código de números randômicos implícito. Na realidade, para escrever geradores para nosso próprio tipo, usamos qualquer conjunto de funções definidas na biblioteca para introduzir novos valores randômicos, para posteriormente juntá-los para construir estruturas de dados nas quais estejamos interessantes. Os tipos da principais funções existentes no QuickCheck são:`

```haskell
-- Defined in ‘Test.QuickCheck.Gen’
elements :: [a] -> Gen a
choose   :: Random a => (a, a) -> Gen a
oneof    :: [Gen a] -> Gen a
```

A função `elements`, por exemplo, recebe uma lista de valores e retorna um gerador de valores randômicos a partir daquela lista. Usaremos `choose` e `oneof` depois. Com isso, podemos começar a escrever realmente nossos geradores para tipos de dados simples. Por exemplo, considere um novo tipo de dado para a lógica ternária:

```haskell
\-- file: rwhptbr/Ch11.hs
data Ternary
    = Yes
    | No
    | Unknown
    deriving (Eq,Show)
```

Podemos escrever uma instância de Arbitrary para o tipo Ternary definindo uma função que escolhe um elemento da lista dos possíveis valores do tipo Ternary:

```haskell
\-- file: ch11/Arbitrary.hs
instance Arbitrary Ternary where
  arbitrary     = elements [Yes, No, Unknown]
```

Another approach to data generation is to generate values for one of the basic Haskell types and then translate those values into the type you're actually interested in. We could have written the Ternary instance by generating integer values from 0 to 2 instead, using `choose`, and then mapping them onto the ternary values: 

\-- file: ch11/Arbitrary2.hs
instance Arbitrary Ternary where
    --arbitrary     = elements [Yes, No, Unknown]
    arbitrary     = do
        n <- choose (0, 2) :: Gen Int
        return $ case n of
                      0 -> Yes
                      1 -> No
                      _ -> Unknown

[2 comments](comments: show / hide)

For simple _sum_ types, this approach works nicely, as the integers map nicely onto the constructors of the data type. For _product_ types (such as structures and tuples), we need to instead generate each component of the product separately (and recursively for nested types), and then combine the components. For example, to generate random pairs of random values: [No comments](comment: add)

\-- file: ch11/Arbitrary.hs
instance (Arbitrary a, Arbitrary b) => Arbitrary (a, b) where
  arbitrary = do
      x <- arbitrary
      y <- arbitrary
      return (x, y)

[2 comments](comments: show / hide)

So let's now write a generator for all the different variants of the Doc type. We'll start by breaking the problem down, first generating random constructors for each type, then, depending on the result, the components of each field. The most complicated case are the union and concatenation variants. [2 comments](comments: show / hide)

First, though, we need to write an instance for generating random characters — QuickCheck doesn't have a default instance for characters, due to the abundance of different text encodings we might want to use for character tests. We'll write our own, and, as we don't care about the actual text content of the document, a simple generator of alphabetic characters and punctuation will suffice (richer generators are simple extensions of this basic approach): [1 comment](comments: show / hide)

\-- file: ch11/QC.hs
instance Arbitrary Char where
    arbitrary = elements (\['A'..'Z'\] ++ \['a' .. 'z'\] ++ " ~!@#$%^&\*()")

[6 comments](comments: show / hide)

With this in place, we can now write an instance for documents, by enumerating the constructors, and filling the fields in. We choose a random integer to represent which document variant to generate, and then dispatch based on the result. To generate concat or union document nodes, we just recurse on `arbitrary`, letting type inference determine which instance of `Arbitrary` we mean: [1 comment](comments: show / hide)

\-- file: ch11/QC.hs
instance Arbitrary Doc where
    arbitrary = do
        n <- choose (1,6) :: Gen Int
        case n of
             1 -> return Empty

             2 -> do x <- arbitrary
                     return (Char x)

             3 -> do x <- arbitrary
                     return (Text x)

             4 -> return Line

             5 -> do x <- arbitrary
                     y <- arbitrary
                     return (Concat x y)

             6 -> do x <- arbitrary
                     y <- arbitrary
                     return (Union x y)

[5 comments](comments: show / hide)

That was fairly straightforward, and we can clean it up some more by using the `oneof` function, whose type we saw earlier, to pick between different generators in a list (we can also use the monadic combinator, `liftM` to avoid naming intermediate results from each generator): [3 comments](comments: show / hide)

\-- file: ch11/QC.hs
instance Arbitrary Doc where
    arbitrary =
        oneof \[ return Empty
              , liftM  Char   arbitrary
              , liftM  Text   arbitrary
              , return Line
              , liftM2 Concat arbitrary arbitrary
              , liftM2 Union  arbitrary arbitrary \]

[1 comment](comments: show / hide)

The latter is more concise, just picking between a list of generators, but they describe the same data either way. We can check that the output makes sense, by generating a list of random documents (seeding the pseudo-random generator with an initial seed of 2): [7 comments](comments: show / hide)

    ghci> 

[8 comments](comments: show / hide)

Looking at the output we see a good mix of simple, base cases, and some more complicated nested documents. We'll be generating hundreds of these each test run, so that should do a pretty good job. We can now write some generic properties for our document functions. [No comments](comment: add)

### Testing document construction

Two of the basic functions on documents are the null document constant (a nullary function), `empty`, and the append function. Their types are: [3 comments](comments: show / hide)

\-- file: ch11/Prettify2.hs
empty :: Doc
(<>)  :: Doc -> Doc -> Doc

[No comments](comment: add)

Together, these should have a nice property: appending or prepending the empty list onto a second list, should leave the second list unchanged. We can state this invariant as a property: [1 comment](comments: show / hide)

\-- file: ch11/QC.hs
prop\_empty\_id x =
    empty <> x == x
  &&
    x <> empty == x

[No comments](comment: add)

Confirming that this is indeed true, we're now underway with our testing: [2 comments](comments: show / hide)

    ghci> 

[4 comments](comments: show / hide)

To look at what actual test documents were generated (by replacing `quickCheck` with `verboseCheck`). A good mixture of both simple and complicated cases are being generated. We can refine the data generation further, with constraints on the proportion of generated data, if desirable. [8 comments](comments: show / hide)

Other functions in the API are also simple enough to have their behaviour fully described via properties. By doing so we can maintain an external, checkable description of the function's behaviour, so later changes won't break these basic invariants. [No comments](comment: add)

\-- file: ch11/QC.hs

prop\_char c   = char c   == Char c

prop\_text s   = text s   == if null s then Empty else Text s

prop\_line     = line     == Line

prop\_double d = double d == text (show d)

[2 comments](comments: show / hide)

These properties are enough to fully test the structure returned by the basic document operators. To test the rest of the library will require more work. [No comments](comment: add)

### Using lists as a model

Higher order functions are the basic glue of reusable programming, and our pretty printer library is no exception — a custom fold function is used internally to implement both document concatenation and interleaving separators between document chunks. The `fold` defined for documents takes a list of document pieces, and glues them all together with a supplied combining function: [2 comments](comments: show / hide)

\-- file: ch11/Prettify2.hs
fold :: (Doc -> Doc -> Doc) -> \[Doc\] -> Doc
fold f = foldr f empty

[No comments](comment: add)

We can write tests in isolation for specific instances of fold easily. Horizontal concatenation of documents, for example, is easy to specify by writing a reference implementation on lists: [No comments](comment: add)

\-- file: ch11/QC.hs

prop\_hcat xs = hcat xs == glue xs
    where
        glue \[\]     = empty
        glue (d:ds) = d <> glue ds

[2 comments](comments: show / hide)

It is a similar story for `punctuate`, where we can model inserting punctuation with list interspersion (from `Data.List`, `intersperse` is a function that takes an element and interleaves it between other elements of a list): [No comments](comment: add)

\-- file: ch11/QC.hs

prop\_punctuate s xs = punctuate s xs == intersperse s xs

[2 comments](comments: show / hide)

While this looks fine, running it reveals a flaw in our reasoning: [No comments](comment: add)

    ghci> 

[No comments](comment: add)

The pretty printing library optimises away redundant empty documents, something the model implementation doesn't, so we'll need to augment our model to match reality. First, we can intersperse the punctuation text throughout the document list, then a little loop to clean up the `Empty` documents scattered through, like so: [3 comments](comments: show / hide)

\-- file: ch11/QC.hs
prop\_punctuate' s xs = punctuate s xs == combine (intersperse s xs)
    where
        combine \[\]           = \[\]
        combine \[x\]          = \[x\]

        combine (x:Empty:ys) = x : combine ys
        combine (Empty:y:ys) = y : combine ys
        combine (x:y:ys)     = x \`Concat\` y : combine ys

[2 comments](comments: show / hide)

Running this in GHCi, we can confirm the result. It is reassuring to have the test framework spot the flaws in our reasoning about the code — exactly what we're looking for: [No comments](comment: add)

    ghci> 

[1 comment](comments: show / hide)

### Putting it altogether

We can put all these tests together in a single file, and run them simply by using one of QuickCheck's driver functions. Several exist, including elaborate parallel ones. The basic batch driver is often good enough, however. All we need do is set up some default test parameters, and then list the functions we want to test: [3 comments](comments: show / hide)

\-- file: ch11/Run.hs
import Prettify2
import Test.QuickCheck.Batch

options = TestOptions
      { no\_of\_tests         = 200
      , length\_of\_tests     = 1
      , debug\_tests         = False }

main = do
    runTests "simple" options
        \[ run prop\_empty\_id
        , run prop\_char
        , run prop\_text
        , run prop\_line
        , run prop\_double
        \]

    runTests "complex" options
        \[ run prop\_hcat
        , run prop\_puncutate'
        \]

[14 comments](comments: show / hide)

We've structured the code here as a separate, standalone test script, with instances and properties in their own file, separate to the library source. This is typical for library projects, where the tests are kept apart from the library itself, and import the library via the module system. The test script can then be compiled and executed: [1 comment](comments: show / hide)

    $ 

[No comments](comment: add)

A total of 1400 individual tests were created, which is comforting. We can increase the depth easily enough, but to find out exactly how well the code is being tested we should turn to the built in code coverage tool, HPC, which can state precisely what is going on. [1 comment](comments: show / hide)

Measuring test coverage with HPC
--------------------------------

HPC (Haskell Program Coverage) is an extension to the compiler to observe what parts of the code were actually executed during a given program run. This is useful in the context of testing, as it lets us observe precisely which functions, branches and expressions were evaluated. The result is precise knowledge about the percent of code tested, that's easy to obtain. HPC comes with a simple utility to generate useful graphs of program coverage, making it easy to zoom in on weak spots in the test suite. [No comments](comment: add)

To obtain test coverage data, all we need to do is add the `-fhpc` flag to the command line, when compiling the tests: [2 comments](comments: show / hide)

    $ ghc -fhpc Run.hs --make
  

[No comments](comment: add)

Then run the tests as normal; [No comments](comment: add)

    $ ./Run
                 simple : .....                            (1000)
                complex : ..                               (400)

   

[No comments](comment: add)

During the test run the trace of the program is written to .tix and .mix files in the current directory. Afterwards, these files are used by the command line tool, `hpc`, to display various statistics about what happened. The basic interface is textual. To begin, we can get a summary of the code tested during the run using the `report` flag to `hpc`. We'll exclude the test programs themselves, (using the `--exclude` flag), so as to concentrate only on code in the pretty printer library. Entering the following into the console: [No comments](comment: add)

    $ hpc report Run --exclude=Main --exclude=QC
     18% expressions used (30/158)
      0% boolean coverage (0/3)
           0% guards (0/3), 3 unevaluated
         100% 'if' conditions (0/0)
         100% qualifiers (0/0)
     23% alternatives used (8/34)
      0% local declarations used (0/4)
     42% top-level declarations used (9/21)
    

[No comments](comment: add)

we see that, on the last line, 42% of top level definitions were evaluated during the test run. Not too bad for a first attempt. As we test more and more functions from the library, this figure will rise. The textual version is useful for a quick summary, but to really see what's going on it is best to look at the marked up output. To generate this, use the `markup` flag instead: [No comments](comment: add)

    $ hpc markup Run --exclude=Main --exclude=QC
  

[No comments](comment: add)

This will generate one html file for each Haskell source file, and some index files. Loading the file `hpc_index.html` into a browser, we can see some pretty graphs of the code coverage: [No comments](comment: add)

![Revised coverage for module Prettify2: 52% of top level definitions (up from 42%), 23% of alternatives, 18% of expressions.](figs/ch11-hpc-round1.png)

Not too bad. Clicking through to the pretty module itself, we see the actual source of the program, marked up in bold yellow for code that wasn't tested, and code that was executed simply bold. [No comments](comment: add)

![Screenshot of annotated coverage output, displaying the Monoid instance for Doc in bold yellow (not tested), and other code nearby in bold (was executed).](figs/ch11-coverage-screen.png)

We forgot to test the Monoid instance, for example, and some of the more complicated functions. HPC helps keep our test suite honest. Let's add a test for the typeclass instance of Monoid, the class of types that support appending and empty elements: [6 comments](comments: show / hide)

\-- file: ch11/QC.hs
prop\_mempty\_id x =
    mempty \`mappend\` x == x
  &&
    x \`mappend\` mempty == (x :: Doc)

[2 comments](comments: show / hide)

Running this property in ghci, to check it is correct: [No comments](comment: add)

    ghci> 

[No comments](comment: add)

We can now recompile and run the test driver. It is important to remove the old .tix file first though, or an error will occur as HPC tries to combine the statistics from separate runs: [No comments](comment: add)

  $ ghc -fhpc Run.hs --make -no-recomp
  $ ./Run 
  Hpc failure: inconsistent number of tick boxes
  (perhaps remove Run.tix file?)
  $ rm \*.tix
  $ ./Run   
                     simple : .....                            (1000)
                    complex : ...                              (600)

[No comments](comment: add)

Another two hundred tests were added to the suite, and our coverage statistics improves to 52 percent of the code base: [No comments](comment: add)

![Coverage for module Prettify2: 42% of top level definitions, 23% of alternatives, 18% of expressions.](figs/ch11-hpc-round2.png)

HPC ensures that we're honest in our testing, as anything less than 100% coverage will be pointed out in glaring color. In particular, it ensures the programmer has to think about error cases, and complicated branches with obscure conditions, all forms of code smell. When combined with a saturating test generation system, like QuickCheck's, testing becomes a rewarding activity, and a core part of Haskell development. [2 comments](comments: show / hide)

  

* * *

\[[27](#id628218)\] Throughout this chapter we'll use QuickCheck 1.0 (classic QuickCheck). It should be kept in mind that a some functions may differ in later releases of the library.

\[[28](#id628795)\] The class also defines a method, `coarbitrary`, which given a value of some type, yields a function for new generators. We can disregard for now, as it is only needed for generating random values of function type. One result of disregarding `coarbitrary` is that GHC will warn about it not being defined, however, it is safe to ignore these warnings.
