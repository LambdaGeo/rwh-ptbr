
[Sumário](index)

## Capítulo 11. Testes e garantia de qualidade         


Construir sistemas reais significa ter cuidado com o controle de qualidade, robustez e corretude. Com os mecanismos certos para a garantia de qualidade, código bem-escrito pode parecer uma máquina precisa, com todas as funções executando suas tarefas de acordo com as especificações. Não há desleixo nas situações críticas o que resulta em código que é auto-explicativo – e obviamente correto – do tipo que inspira confiança.

Em Haskell, existem diversas ferramentas à disposição para construir tais sistemas precisos. A ferramenta mais óbvia, e construída na própria linguagem é o sistema de tipos expressivo, garante que toda verificação seja feita estaticamente – tornando impossível escrever código que viole tais restrições. Adicionalmente, pureza e polimorfismo promovem um estilo de código que é modular, refatorável e testável. Este é o tipo de código que não contém erros.

Os testes possuem um papel importante para manter o código no caminho certo. Os principal mecanismo de teste em Haskell são os tradicionais teste de unidade (por meio da biblioteca HUnit) e o seu descendente mais poderoso, teste baseado em propriedades através do QuickCheck, um framework de testes de código-livre para Haskell. Testes baseados em propriedades promovem uma abordagem de alto-nível para os testes na forma de funções invariantes que devem satisfazer universalmente, com os dados reais de testes gerados pela biblioteca para o progamador. Desta forma, o código pode ser exaustivamente testado com milhares de testes que iriam ser inviáveis para escrever manualmente, geralmente não cobrindo casos especiais que não seriam encontrados de outra forma.

Neste capítulo iremos ver como usar QuickCheck para estabelecer invariantes no código, e então re-examinar o "pretty printer" desenvolvido nos capítulos anteriores, testando-o com o framework. Iremos também ver como conduzir o processo de testes com a ferramenta de cobertura de testes do GHC: HPC.

Caso não tenha desenvolvido a biblioteca pretty-printer do capítulo 5, clone ou baixe a partir do seguinte repositório:

```
git clone https://github.com/profsergiocosta/hs2json.git
```
Após clonado, entre na pasta da biblioteca e executa os testes:

```
$cd hs2json
$stack test
```
Após a compilação irá aparecer a informação que ainda não existe testes implementados:

```
hs2json-0.1.0.0: test (suite: hs2json-test)
                             
Test suite not yet implemented

hs2json-0.1.0.0: Test suite hs2json-test passed
```
Podemos verificar no código fonte que nenhum teste foi implementado:

```haskell
\-- file: test/Spec.hs
main :: IO ()
main = putStrLn "Test suite not yet implemented"
```

O objetivo deste capítulo é implementarmos estes testes. Para tanto, eu irei guardar essa implementação em um novo repositório, para que vocês possam utiliza-lo como referência se algo der errado. Porém, é importante que execute os passos a seguir a partir deste repositório sem os testes implmentados. Então, primeiro removi o repositorio atual do remote e depois associei a outro repositório
```
$git remote rm origin
$git remote add origin git@github.com:profsergiocosta/hs2json-test.git
$git push origin master
```

### QuickCheck: Teste baseado em tipos

Para obter uma ideia geral sobre como funcionam os testes baseado em tipos, iremos começar com um cenário simples: você escreveu uma função específica de ordenação e deseja testar o seu comportamento. Para entendermos melhor, criem um novo módulo `QuickTestes` aonde iremos usar um novo tipo de dado para a lógica ternária:

```haskell
-- file: src/QuickTestes.hs
module QuickTestes where
```
E a função que nós queremos testar – uma rotina personalizada de ordenação:: 

```haskell
-- file: src/QuickTestes.hs
qsort :: Ord a => [a] -> [a]
qsort []     = []
qsort (x:xs) = qsort lhs ++ [x] ++ qsort rhs
    where lhs = filter  (< x) xs
          rhs = filter (>= x) xs
```

Esta é a clássica implementação de ordenação em Haskell: um estudo sobre a elegância em programação funcional, não em eficiência (este não é um algoritmo de ordenação in-place, que altera a estrutura). Agora, nós queremos checar se esta função obedece às regras básicas que uma boa ordenação deveria seguir. Uma invariante útil para começar e uma que aparece com frequência em códigos puramente funcionais, é a idempotência – uma função aplicada duas vezes deve ter o mesmo resultado quando aplicada apenas uma vez. Para a nossa rotina de ordenação – um algoritmo estável de ordenação – isso deve ser sempre verdadeiro, ou a situação irá ficar feia. A invariante pode ser codificada como uma simples propriedade, da seguinte maneira

```haskell
-- file: src/RandomTest.hs
prop_idempotent xs = qsort (qsort xs) == qsort xs
```

Iremos usar a conveção de QuickCheck de prefixar as propriedades de teste com `prop_` para diferenciá-las de código normal. A propriedade de idempotência é escrita simplesmente como uma função Haskell declarando uma igualdade que deve valer para todos os dados da entrada que é ordenada. Podemos checar manualmente se isso faz sentido para alguns casos simples:

```
$stack ghci
ghci> prop_idempotent []       
True
ghci> prop_idempotent [1,1,1,1]  
True
ghci> prop_idempotent [1..100]
True
ghci> prop_idempotent [1,5,2,1,2,0,9]
True
```
Parece estar certo. Entretanto, escrever os dados de entrada à mão é tedioso e viola o código moral dos programadores funcionais eficientes: deixe a máquina fazer o trabalho! Para automatizar isto, a biblioteca QuickCheck provê um conjunto de geradores de dados para todos os tipos de dados básicos do Haskell. QuickCheck usa o `typeclass` Arbitrary para apresentar uma interface uniforme a um pseudo aleatório gerador de dados com o tipo do sistema usado para resolver a questão de qual gerador usar. QuickCheck normalmente esconde o funcionamento da geração de dados, entretanto, nós podemos também executar os geradores à mão para obter uma ideia dos dados que o QuickCheck produz. Primeiro, é necessário incluí-lo nas dependecias:

```yaml
#file: package.yaml
dependencies:
- base >= 4.7 && < 5
- QuickCheck
```

Por exemplo, gerar uma lista aleatória de valores booleanos:

```
$stack ghci
ghci>:module +Test.QuickCheck
ghci> generate  arbitrary :: IO [Bool]
[False,True,True,True,False,True,False,True]
```


QuickCheck gera dados de teste desta maneira e os passa à propriedade de nossa escolha, por meio da função quickCheck. O tipo da propriedade em si determina qual gerador de dados é usado. O `quickCheck` então checa para todos os dados de teste produzido, que a propriedade foi satisfeita. Agora, uma vez que nosso teste de idempotência é polimórfico na lista de tipos de elementos, precisamos escolher um tipo particular para o qual desejamos gerar os dados de teste, o qual iremos escrever como uma restrição de tipo da propriedade. Para executar o teste, apenas chamando quickCheck com a nossa função de propriedade, que está configurada para o tipo de dado requerido (caso contrário, o tipo de elemento da lista irá ser o padrão desinteressante tipo ())

```
ghci>quickCheck (prop_idempotent :: [Integer] -> Bool)
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
-- file: src/QuickTestes.hs
prop_minimum xs         = head (qsort xs) == minimum xs
```
Ao recarregar o moduloe e testar essa nova propriendade, iremos indentificar um erro:
```
ghci>:r
ghci> quickCheck (prop_minimum :: [Integer] -> Bool)
*** Failed! Exception: 'Prelude.head: empty list' (after 1 test):
[]
```

A propriedade falhou quando ordenou uma lista vazia, para a qual head e minimum não estão definidas, como podemos ver pela sua definição:

```haskell
-- defined in Prelude.hs
head :: [a] -> a
head (x:_) = x
head [] = error "Prelude.head: empty list"

minimum :: (Ord a) => [a] -> a
minimum [] = error "Prelude.minimum: empty list"
minimum xs = foldl1 min xs
```
Portanto esta propriedade irá funcionar apenas para listas não-vazias. QuickCheck, felizmente, vem com uma linguagem própria para escrever propriedades, para que possamos especificar mais precisamente nossas invariantes, removendo valores que não queremos considerar. Para o caso da lista vazia, nós realmente queremos dizer que se a lista não está vazia, então o primeiro elemento da lista ordenada é o menor da lista de entrada. Isto é feito utilizando a função de implicação(`==>`), que remove dados inválidos antes de executar as propriedades:

```haskell
-- file: src/QuickTestes.hs
prop_minimum' xs         = not (null xs) ==> head (qsort xs) == minimum xs
```
O resultado é claro. Removendo o caso da lista vazia, podemos confirmar que a propriedade de fato funciona:

```
ghci> quickCheck (prop_minimum' :: [Integer] -> Property)
+++ OK, passed 100 tests; 17 discarded.
```
Note que tivemos que mudar o tipo da propriedade, anteriormente sendo um simples resultado Bool para agora ser um resultado mais geral do tipo Property(a propriedade em si agora é uma função que remove listas vazias, antes de testá-las, ao invés de uma simples constante booleana).

Podemos agora completar o conjunto básico de propriedades para a função de ordenação com outras invariantes que ela deve satisfazer: a saída deve ser ordenada (cada elemento deve ser menor, ou igual, ao seu sucessor); a saída deve ser uma permutação da entrada (a qual nós alcançamos através da função diferença de lista, `(\\)`); o último elemento ordenado deve ser o maior elemento; e se encontramos o menor elemento de duas listas, ele deve ser o primeiro elemento se juntarmos e ordenarmos tais listas. Estas propriedades podem ser definidas como:

```haskell
-- file: src/QuickTestes.hs
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
-- file: src/QuickTestes.hs
prop_sort_model xs      = sort xs == qsort xs
```

Este tipo de teste baseado em modelo é extremamente poderoso. Geralmente, desenvolvedores irão ter uma implementação de referência ou protótipo que, embora ineficiente, é correta. Isso pode então ser mantido por perto e usado para assegurar que o código de produção otimizado está de acordo com a referência. Ao construir uma grande suíte desses testes baseados em modelos e executando-os regularmente(em cada commit, por exemplo), podemos facilmente assegurar a precisão de nosso código. Grandes projetos Haskell geralmente possuem suítes de propriedades de tamanho comparável com o próprio projeto, com milhares de invariantes testadas em cada mudança, mantendo o código de acordo com a especificação e assegurando que ele se comporta como requerido.

### Caso de estudo de teste: especificando uma `pretty printer`

Testar as propriedades naturais de  funções individuais é um das mais básicas abordagens que guiam o desenvolvimento de grandes sistemas em Haskell. Veremos agora um cenário mais complicado: construir uma suíte de testes para a biblioteca de pretty-printing* desenvolvida em capítulos anteriores.

*N.dT.: Pretty-printing é o nome que se dá à apresentação de um conteúdo de maneira em que a estrutura da apresentação intensifica o sentido do próprio conteúdo


#### Gerando dados de teste

Lembre-se que a pretty printer é construída de acordo com o Doc, um tipo de dado algébrico que representa documentos bem-estruturados.

```haskell
\-- file: src/Prettify.hs
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

A função `elements`, por exemplo, recebe uma lista de valores e retorna um gerador de valores randômicos a partir daquela lista. Usaremos `choose` e `oneof` depois. Com isso, podemos começar a escrever realmente nossos geradores para tipos de dados simples. Para entendermos melhor, iremos usar novamene o módulo `QuickTestes` aonde iremos usar um novo tipo de dado para a lógica ternária:

```haskell
-- file: src/QuickTestes.hs
data Ternary
    = Yes
    | No
    | Unknown
    deriving (Eq,Show)
```

Podemos escrever uma instância de Arbitrary para o tipo Ternary definindo uma função que escolhe um elemento da lista dos possíveis valores do tipo Ternary:

```haskell
-- file: src/RandomTest.hs
instance Arbitrary Ternary where
  arbitrary     = elements [Yes, No, Unknown]
```

Com essa implementação já é possível gerar dados aleatórios para este tipo de dados:

ghci>:r
ghci>generate  arbitrary :: IO [Ternary]
[Unknown,Yes,Yes,Yes,Unknown,Yes,Unknown,Unknown,Unknown,No,No,Yes,Yes]

Outra abordagem para a geração de dados é gerar valores para um dos tipos básicos de Haskell e traduzir tais valores em tipos nos quais estejamos interessados. Poderíamos ter escrito a instância de Ternary gerando valores inteiros de 0 a 2 por exemplo, usando `choose`, e então mapeando os para valores ternários:

```haskell
-- file: src/RandomTest.hs
instance Arbitrary Ternary where
    --arbitrary     = elements [Yes, No, Unknown]
    arbitrary     = do
        n <- choose (0, 2) :: Gen Int
        return $ case n of
                      0 -> Yes
                      1 -> No
                      _ -> Unknown
```

Para tipos enumerados, essa abordagem funciona bem, já que os inteiros são facilmente mapeáveis para os construtores do tipo de dado. Para tipos cartesianos (como as estruturas e as tuplas), precisamos de, no lugar, gerar cara componente do produto separadamente (e recursivamente para tipos aninhados), e então combinar os componentes. Por exemplo, para gerar pares de valores randômicos:

```haskell
-- Defined in ‘Test.QuickCheck.Arbitrary’
instance (Arbitrary a, Arbitrary b) => Arbitrary (a, b) where
  arbitrary = do
      x <- arbitrary
      y <- arbitrary
      return (x, y)
```

Por exemplo, poderiamos testar com uma tupla de inteiros:

```
ghci> generate  arbitrary :: IO [(Int,Int)]
[(27,-24),(-3,-13),(17,24),(28,-1),(-24,5),(-14,-25)]
```

Vamos escrever um gerador para todas as diferentes variantes do tipo Doc. Começaremos quebrando o problema em problemas menores, inicialmente gerando construtores randômicos para cada tipo, e então, dependendo do resultado, os componentes de cada campo. Os casos mais complicados são as variantes de concatenação e união.

Quando este livro foi escrito originalmente, o QuickCheck não tem uma instância padrão para caracteres, devido à abundância de diferentes codificações de texto que podemos querer usar para testes de caracteres. Nas versões mais recentes já existe essa implementação definida em `Test.QuickCheck`:

```haskell
-- Defined in ‘Test.QuickCheck.Arbitrary’
instance Arbitrary Char where
    arbitrary = elements (['A'..'Z'] ++ ['a' .. 'z'] ++ " ~!@#$%^&*()")
```

Com isso, podemos agora escrever uma instância para documentos enumerando os construtores e preenchendo os campos. Escolhemos um inteiro randômico para representar qual variante do documento será gerada, e então realizar a escolha baseada no resultado. Para gerar nós de documentos de concatenação ou união, usamos recursão sobre arbitrary, deixando a inferência de tipos determinar qual instância de Arbitrary desejamos. Iremos para tanto, escrever o código em test/spec.hs:

```haskell
-- file: test/Spec.hs
import Test.QuickCheck
import Data.List
import Prettify

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

main :: IO ()
main = do
    docs <- generate  arbitrary :: IO [Doc]
    print docs
    docs <- generate  arbitrary :: IO [Doc]
    print docs
    docs <- generate  arbitrary :: IO [Doc]
    print docs
```

Com este código, já é possível executar o "teste":
```
$ stack test
hs2json-0.1.0.0: test (suite: hs2json-test)

[Text "* a\aS\770726\104944\772241Z\693704\594893\1079094\195229\133711\RS5IM\202165+T\STXh\578922\aCl*",Concat Line (Concat Empty (Char '4')),Char '\NUL',Concat Line (Concat Line (Concat (Text ">LJ\704988\96698i\325142X\GS\192746\GS\830877\EOT\1112145I\724715\448100^\DC4\SUBG.\SYN%\380946") (Union Line (Char 'm'))))]
[Union (Union (Concat (Char 'X') Line) (Union (Concat (Text "\773866 =\988211L\926094LZ\SUBb2") (Char '{')) (Concat(Char 'g') (Union Line Line)))) Empty,Concat (Char ')') Empty,Char 'z',Empty,Empty,Empty,Union Empty (Text "U!\DEL\706669\1009497'"),Union (Concat (Text "\645294=\1058950;0ku^") (Char '\520057')) (Concat (Concat Empty (Concat (Char'\338637') Empty)) (Concat (Char '\DEL') (Union (Union Line Empty) Empty))),Char 'e']
[Text "\961163\434986A\131685\642589sK<\366215LB\698621aw{v/m\971482TO\RS\ETX%\RS}\US\vL"]
```
Examinando a saída, vemos uma boa mistura de casos básicos e alguns documentos aninhados mais complicados. Geraremos centenas desde a cada execução de teste para que o teste seja válido. Agora podemos escrever algumas propriedades genéricas para nossas funções. Essa foi uma abordagem bem direta, e podemos melhorá-la um pouco mais usando a função `oneof`, cujo tipo vimos anteriormente, para escolher entre diferentes geradores em uma lista (podemos usar também o combinador monádico, `liftM`, para evitar nomear resultados intermediários de cada gerador):

```haskell
-- file: test/Spec.hs
instance Arbitrary Doc where
    arbitrary =
        oneof [ return Empty
              , liftM  Char   arbitrary
              , liftM  Text   arbitrary
              , return Line
              , liftM2 Concat arbitrary arbitrary
              , liftM2 Union  arbitrary arbitrary ]

```
Para usar o combinador `liftM` foi importado o módulo `Control.Monad`. Esta última versão é mais concisa – escolhendo apenas de uma lista de geradores – embora ambas as versões descrevam os mesmo dados. Podemos checar que a saída faz sentido, ao gerar uma lista de documentos randômicos (escolhemos a semente inicial do gerador pseudo-randômico como 2).

#### Testando a Construção de Documentos

Duas das funções básicas sobre documentos são a constante de documento nulo (função nulária), empty, e a função anexar. Revendo suas definições:

```haskell
-- file: src/Prettify.hs
empty :: Doc
empty = Empty

(<>) :: Doc -> Doc -> Doc
Empty <> y = y
x <> Empty = x
x <> y = x `Concat` y

```

Juntas, essas funções deveriam compor uma propriedade razoável: anexar ou prepor uma lista vazia a uma segunda lista deveria deixar a segunda lista inalterada. Podemos afirmar essa invariante como uma propriedade:

```haskell
-- file: test/Spec.hs
prop_empty_id x =
    empty <> x == x
  &&
    x <> empty == x
```
Ao confirmar que essa propriedade é verdadeira, podemos continuar a criação de nossos testes:

```
ghci> quickCheck prop_empty_id
+++ OK, passed 100 tests.
```

(REESCREVER ESSA PARTE)O Haskell provê um template para facilitar a aplicação dos testes. Para tanto, é necessário fazer as seguinte alterações no arquivo:


```haskell
-- file: test/Spec.hs

{-# LANGUAGE TemplateHaskell #-}

import Test.QuickCheck
import Data.List
import Prettify
import Control.Monad
import Prelude hiding ((<>), empty)

instance Arbitrary Doc where
    arbitrary =
        oneof [ return Empty
              , liftM  Char   arbitrary
              , liftM  Text   arbitrary
              , return Line
              , liftM2 Concat arbitrary arbitrary
              , liftM2 Union  arbitrary arbitrary ]

prop_empty_id x =
    empty <> x == x
  &&
    x <> empty == x


return []
runTests = $quickCheckAll
    
main :: IO ()
main = runTests >>= \passed -> if passed then putStrLn "Passou em todos testes."
                                             else putStrLn "Alguns testes falharam"
```

O quickcheck irá avaliar todas as funções que iniciam com `prop_`. Então, a seguinte linha de código irá fazer o trabalho de buscar todas essas funções:

```haskell
-- file: rwhptbr/Ch11.hs
return []
runTests = $quickCheckAll
```

Tudo o que precisamos é executar todos os testes e verificar se passou em todos os testes:

```haskell
-- file: test/Spec.hs
main :: IO ()
main = runTests >>= \passed -> if passed then putStrLn "All tests passed."
                                         else putStrLn "Some tests failed."
```

Com essas modificações, podemos então executar nossos testes. Observe novamente que para o quickCheckAll executar os testes, as funções precisam iniciar com `prop_`.

```
$stack test
hs2json-0.1.0.0: test (suite: hs2json-test)

=== prop_empty_id from test/Spec.hs:19 ===
+++ OK, passed 100 tests.

Passou em todos testes.

hs2json-0.1.0.0: Test suite hs2json-test passed
Completed 2 action(s).
```

Outras funções na API são simples o suficiente para terem o seu comportamento completamente descrito por propriedades são as seguintes:


```haskell
-- file: test/Spec.hs
char :: Char -> Doc
char c = Char c

text :: String -> Doc
text "" = Empty
text s  = Text s

double :: Double -> Doc
double d = text (show d)

line :: Doc
line = Line
```

Fazemos então os seguintes testes para estas funções básicas, de modo que modificações futuras não irão quebras essas invariantes básicas.

```haskell
-- file: test/Spec.hs
prop_char c   = char c   == Char c
prop_text s   = text s   == if null s then Empty else Text s
prop_line     = line     == Line
prop_double d = double d == text (show d)
```

Essas propriedades são suficientes para testar completamente a estrutura retornada pelos operadores básicos de documentos. Testar o restante da biblioteca requer mais esforço que ficará a cargo do leitor :)

#### Usando listas como modelos

Funções de alta ordem são a base de programas reusáveis, e a nossa biblioteca de pretty-printing não é exceção – uma função `fold` customizada é usada internamente para implementar tanto concatenação quanto intercalação de separadores entre pedaços de documentos. O `fold` definido para documentos recebe uma lista de pedaços de documentos e os uni de acordo com uma função de combinação:

```haskell
-- file: src/Pretiffy.hs
fold :: (Doc -> Doc -> Doc) -> [Doc] -> Doc
fold f = foldr f empty

hcat :: [Doc] -> Doc
hcat = fold (<>)
```

Podemos escrever testes em isolamento para instâncias específicas de fold facilmente. A concatenação horizontal de documentos, por exemplo, é fácil de ser especificada escrevendo-se uma implementação de referência sobre listas:

```haskell
-- file: test/Spec.hs
prop_hcat xs = hcat xs == glue xs
    where
        glue []     = empty
        glue (d:ds) = d <> glue ds
```

Acontece uma história similar com `punctuate`, onde podemos modelar a inserção de pontuação com intercalação de listas (`intersperse`, de Data.List,é uma função que recebe um elemento e o intercala entre outros elementos da lista):

```haskell
-- file: test/Spec.hs
prop_punctuate s xs = punctuate s xs == intersperse s xs
```

Embora pareça correta, a execução revela uma falha na nossa lógica:

```
=== prop_punctuate from test/Spec.hs:39 ===
*** Failed! Falsifiable (after 5 tests and 1 shrink):
Char 'y'
[Char '\EOT',Char 'f']

Alguns testes falharam

hs2json-0.1.0.0: Test suite hs2json-test passed
Completed 2 action(s).
```

A biblioteca de pretty-printing otimiza documentos vazios redundantes, algo que o modelo de implementação não faz, logo precisaremos aumentar o nosso modelo para satisfazer a realidade. Primeiro, intercalamos a pontuação pela lista de documentos, e então eliminamos os documentos Empty espalhados pela lista, desta maneira:

```haskell
-- file: test/Spec.hs
prop_punctuate' s xs = punctuate s xs == combine (intersperse s xs)
    where
        combine []           = []
        combine [x]          = [x]

        combine (x:Empty:ys) = x : combine ys
        combine (Empty:y:ys) = y : combine ys
        combine (x:y:ys)     = x `Concat` y : combine ys
```

Executando isso no GHCi, podemos confirmar o resultado. É reconfortante que o framework de testes consiga localizar falhas em nossa lógica expressa no código – exatamente o que estamos procurando:

```
=== prop_punctuate' from test/Spec.hs:39 ===
+++ OK, passed 100 tests.

Passou em todos testes.

hs2json-0.1.0.0: Test suite hs2json-test passed
Completed 2 action(s).
```

## Medindo a cobertura de teste com HPC

(ESSA PARTE PRECISA SER REESCRITA, DADAS TODAS MUDANÇAS NAS NOVAS VERSOES)

Atualmente esse processo é bem distinto nas versões mais novas, o que irá requerer uma revisão completa desta seção. 


O HPC (Haskell Program Coverage) é uma extensão para que o compilador observe quais partes do código foram realmente executadas durante a execução do programa dado. Isso é útil no contexto de teste, pois nos permite observar com precisão quais as funções, ramos e expressões foram avaliadas. O resultado é um conhecimento preciso sobre o percentual de código testado que é facilmente obtido. O HPC vem com um utilitário simples para gerar gráficos úteis de cobertura do programa, tornando mais fácil para verificar os pontos fracos no conjunto de testes.

Para a obtenção de dados de cobertura de testes, tudo o que precisamos fazer é adicionar o parâmetro coverage que fará gerar um relatório sobre a cobertura dos testes:

```
$ stack test --coverage
hs2json-0.1.0.0: test (suite: hs2json-test)

=== prop_empty_id from test/Spec.hs:23 ===
+++ OK, passed 100 tests.

=== prop_char from test/Spec.hs:29 ===
+++ OK, passed 100 tests.

=== prop_text from test/Spec.hs:30 ===
+++ OK, passed 100 tests.

=== prop_line from test/Spec.hs:31 ===
+++ OK, passed 1 test.

=== prop_double from test/Spec.hs:32 ===
+++ OK, passed 100 tests.

=== prop_hcat from test/Spec.hs:34 ===
+++ OK, passed 100 tests.

=== prop_punctuate' from test/Spec.hs:39 ===
+++ OK, passed 100 tests.

Passou em todos testes.

hs2json-0.1.0.0: Test suite hs2json-test passed
Generating coverage report for hs2json's test-suite "hs2json-test"
 19% expressions used (30/154)
  0% boolean coverage (0/3)
       0% guards (0/3), 3 unevaluated
     100% 'if' conditions (0/0)
     100% qualifiers (0/0)
 23% alternatives used (8/34)
  0% local declarations used (0/4)
 52% top-level declarations used (10/19)
The coverage report for hs2json's test-suite "hs2json-test" is available at /home/sergiosouzacosta/tmp/ch11/hs2json/.stack-work/install/x86_64-linux-tinfo6/lts-13.4/8.6.3/hpc/hs2json/hs2json-test/hpc_index.html
Only one tix file found in /home/sergiosouzacosta/tmp/ch11/hs2json/.stack-work/install/x86_64-linux-tinfo6/lts-13.4/8.6.3/hpc/, so not generating a unified coverage report.

An index of the generated HTML coverage reports is available at /home/sergiosouzacosta/tmp/ch11/hs2json/.stack-work/install/x86_64-linux-tinfo6/lts-13.4/8.6.3/hpc/index.html

```

(FALTA DISCUTIR ESSE RELATÓRIO)
