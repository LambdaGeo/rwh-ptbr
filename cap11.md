Capítulo 11 - Testesol{margin:0;padding:0}p{margin:0}.c17{color:#000000;font-size:12pt;background-color:#f0f4ff;font-family:Verdana}.c1{line-height:1.5;text-indent:36.0pt;text-align:justify;direction:ltr}.c0{line-height:1.5;text-indent:0pt;text-align:justify;direction:ltr}.c11{line-height:1.5;text-indent:0pt;direction:ltr}.c2{color:#000000;font-size:11pt;font-family:Courier New}.c12{line-height:1.5;text-indent:36.0pt;direction:ltr}.c9{color:#000000;font-size:18pt;font-family:Arial}.c10{color:#000000;font-size:14pt;font-family:Arial}.c3{line-height:1.15;text-indent:0pt;direction:ltr}.c8{color:#000000;font-size:8pt;font-family:Arial}.c14{line-height:1.15;text-indent:36.0pt;direction:ltr}.c15{color:#000000;font-size:11pt;font-family:Cambria}.c4{color:#000000;font-size:11pt;font-family:Arial}.c16{background-color:#e7ffc7}.c18{font-weight:bold}.c7{background-color:#ffffff}.c13{margin-left:36.0pt}.c5{font-style:italic}.c6{text-align:justify}

Capítulo 11 – Testes e garantia de qualidade

Construir sistemas reais significa ter cuidado com o controle de qualidade, robustez e precisão. Com os mecanismos certos para a garantia de qualidade, código bem-escrito pode parecer uma máquina precisa, com todas as funções executando suas tarefas de acordo com as especificações. Não há desleixo nas situações críticas o que resulta em código que é auto-explicativo – e obviamente correto – do tipo que inspira confiança.

Em Haskell, existem diversas ferramentas à disposição para construir tais sistemas precisos. A ferramenta mais óbvia, e construída na própria linguagem é o expressive type system, que permite invariantes complicadas serem executadas estaticamente – tornando impossível escrever código que viole tais restrições. Adicionalmente, pureza e polimorfismo promovem um estilo de código que é modular, refatorável e testável. Este é o tipo de código que não contém erros.

Os testes possuem aum importante papel em manter o código no caminho certo. Os tradicionais mecanismos de teste em Haskell são os testes unitários(por meio da biblioteca HUnit) e o seu descendente mais poderoso, teste de propriedades baseado em tipo, com o QuickCheck, um framework de testes de código-livre para Haskell. Testes baseados em propriedades promovem uma abordagem de alto-nível para os testes na forma de funções invariantes abstratas que devem satisfazer universalmente, com os dados reais de testes gerados pela biblioteca para o progamador. Desta forma, o código pode ser exaustivamente testado com milhares de testes que iriam ser inviáveis para escrever com as mãos, geralmente não cobrindo casos especiais que não seriam encontrados.

Neste capítulo iremos ver como usar QuickCheck para estabelecer invariantes no código, e então re-examinar o pretty printer desenvolvido nos capítulos anteriores, testando-o com o framework. Iremos também ver como conduzir o processo de testes com a ferramenta de cobertura de testes do GHC: HPC.

QuickCheck – Teste baseado em tipos

Para obter uma ideia geral sobre como funcionam os testes baseado em tipos, iremos começar com um cenário simples: você escreveu uma função específica de ordenação e deseja testar o seu comportamento.

Primeiramente, nos importamos a biblioteca QuickCheck e os módulos necessários:

\-\- file: ch11/QC-basics.hs

import Test.QuickCheck

import Data.List

E a função que nós queremos testar – uma rotina personalizada de ordenação:

\-\- file: ch11/QC-basics.hs

qsort :: Ord a => \[a\] -> \[a\]

qsort \[\] = \[\]

qsort (x:xs) = qsort lhs ++ \[x\] ++ qsort rhs

where lhs = filter (< x) xs

rhs = filter (>= x) xs

Esta é a clássica implementação de ordenação em Haskell: um estudo sobre a elegância em programação funcional, não em eficiência(este não é um algoritmo de ordenação in-place, que altera a estrutura). Agora, nós queremos checar que esta função obedece às regras básicas que uma boa ordenação deveria seguir. Uma invariante útil para começar e uma que aparece com frequência em códigos puramente funcionais, é a idempotência – uma função aplicada duas vezes deve ter o mesmo resultado quando aplicada apenas uma vez. Para a nossa rotina de ordenação – um algoritmo estável de ordenação – isso deve ser sempre verdadeiro, ou a situação irá ficar feia. A invariante pode ser codificada como uma simples propriedade, da seguinte maneira:

\-\- file: ch11/QC-basics.hs

prop_idempotent xs = qsort (qsort xs) == qsort xs

Iremos usar a conveção de QuickCheck de prefixar as propriedades de teste com prop_ para diferenciá-las de código normal. A propriedade de idempotência é escrita simplesmente como uma função Haskell declarando uma igualdade que deve valer para todos os dados da entrada que é ordenada. Podemos checar à mão que isso faz sentido para alguns casos simples:

ghci> prop_idempotent \[\]        
True  
ghci> prop_idempotent \[1,1,1,1\]    
True  
ghci> prop_idempotent \[1..100\]  
True  
ghci> prop_idempotent \[1,5,2,1,2,0,9\]  
True

Parece estar certo. Entretanto, escrever os dados de entrada à mão é tedioso e viola a o código moral dos programadores funcionais eficientes: deixe a máquina fazer o trabalho! Para automatizar isto, a biblioteca QuickCheck provê um conjunto de geradores de dados para todos os tipos básicos de dados Haskell. QuickCheck usa o tipo Arbitrary para apresentar uma interface uniforme a um pseudo aleatório gerador de dados com o tipo do sistema usado para resolver a questão de qual gerador usar. QuickCheck normalmente esconde o funcionamento da geração de dados, entretanto, nós podemos também executar os geradores à mão para obter uma ideia dos dados que o QuickCheck produz. Por exemplo, gerar uma lista aleatória de valores booleanos:

ghci> generate 10 (System.Random.mkStdGen 2) arbitrary :: \[Bool\] \[False,False,False,False,False,True\]

QuickCheck gera dados de teste desta maneira e os passa à propriedade de nossa escolha, por meio da função quickCheck. O tipo da propriedade em si determina qual gerador de dados é usado. quickCheck então checa para todos os dados de teste produzido, que a propriedade foi satisfeita. Agora, uma vez que nosso teste de idempotência é polimórfico na lista de tipos de elementos, precisamos escolher um tipo particular para o qual desejamos gerar os dados de teste, o qual iremos escrever como uma restrição de tipo da propriedade. Para executar o teste, apenas chamamos quickCheck com a nossa função de propriedade, que está configurada para o tipo de dado requerido(caso contrário, o tipo de elemento da lista irá ser o padrão desinteressante tipo ())

ghci> :type quickCheck

quickCheck :: (Testable a) => a -> IO ()

ghci> quickCheck (prop_idempotent :: \[Integer\] -> Bool)

passed 100 tests.

Para as diferentes 100 listas geradas, a nossa propriedade foi um sucesso. Quando escrever testes, geralmente é útil olhar os reais dados gerados para cada teste. Para fazer isso, iremos substituir quickCheck pelo seu irmão, verboseCheck, para ver a saída de cada teste. Agora, vamos olhar para propriedades sofisticadas que a nossa função deve satisfazer.

Testes de propriedades

Boas bibliotecas consistem de um conjunto de primitivas ortogonais que possuem relações sensíveis entre si. Podemos usar QuickCheck para especificar as relações entre funções no nosso código, o que nos ajuda a encontrar uma boa interface para a biblioteca por meio do desenvolvimento de funções que são interrelacionadas através de propriedades úteis. QuickCheck atua desta maneira como uma ferramenta “lint” de API – ela provê suporte da máquina para assegurar que a nossa API da biblioteca tem sentido.

A função de ordenação de lista deve certamente conter um número de propriedades interessantes que se relacionam com outras operações de lista. Por exemplo, o primeiro elemento em uma lista ordenada deve sempre ser o menor elemento da lista de entrada. Ficamos tentados a especificar essa intuição em Haskell, usando a função minimum da biblioteca List:

\-\- file: ch11/QC-basics.hs

prop_minimum xs = head (qsort xs) == minimum xs

Testando isso, no entanto, revela um erro:

ghci> quickCheck (prop_minimum :: \[Integer\] -> Bool)  
\*\* Exception: Prelude.head: empty listA propriedade falhou quando ordenou uma lista vazia, para a qual head e minimum não estão definidas, como podemos ver pela sua definição:

\-\- file: ch11/minimum.hs

head :: \[a\] -> a

head (x:_) = x

head \[\] = error "Prelude.head: empty list"

minimum :: (Ord a) => \[a\] -> a

minimum \[\] = error "Prelude.minimum: empty list"

minimum xs = foldl1 min xs

Portanto esta propriedade irá funcionar apenas para listas não-vazias. QuickCheck, felizmente, vem com uma extensa linguagem embarcada para escrever propriedades, para que possamos especificar mais precisamente nossas invariantes, removendo valores que não queremos considerar. Para o caso da lista vazia, nós realmente queremos dizer que se a lista não está vazia, então o primeiro elemento da lista ordenada é o menor da lista de entrada. Isto é feito utilizando a função de implicação(==>), que remove dados inválidos antes de executar as propriedades:

\-\- file: ch11/QC-basics.hs  
prop_minimum' xs = not (null xs) ==> head (qsort xs) == minimum xs

O resultado é claro. Removendo o caso da lista vazia, podemos confirmar que a propriedade de fato funciona:

ghci> quickCheck (prop_minimum' :: \[Integer\] -> Property)

passed 100 tests.

Note que tivemos que mudar o tipo da propriedade, anteriormente sendo um simples resultado Bool para agora ser um resultado mais geral do tipo Property(a propriedade em si agora é uma função que remove listas vazias, antes de testá-las, ao invés de uma simples constante booleana).

Podemos agora completar o conjunto básico de propriedades para a função de ordenação com outras invariantes que ela deve satisfazer: a saída deve ser ordenada(cada elemento deve ser menor, ou igual, ao seu sucessor); a saída deve ser uma permutação da entrada(a qual nós alcançamos através da função diferença de lista, (\\\)); o último elemento ordenado deve ser o maior elemento; e se encontramos o menor elemento de duas listas, ele deve ser o primeiro elemento se juntarmos e ordenarmos tais listas. Estas propriedades podem ser definidas como:

\-\- file: ch11/QC-basics.hs  
prop_ordered xs = ordered (qsort xs)  
   where ordered \[\]       = True  
         ordered \[x\]      = True  
         ordered (x:y:xs) = x <= y && ordered (y:xs)  
  
prop_permutation xs = permutation xs (qsort xs)  
   where permutation xs ys = null (xs \\\ ys) && null (ys \\\ xs)  
  
prop_maximum xs         =  
   not (null xs) ==>  
       last (qsort xs) == maximum xs  
  
prop_append xs ys       =  
   not (null xs) ==>  
   not (null ys) ==>  
       head (qsort (xs ++ ys)) == min (minimum xs) (minimum ys)

Testando sobre um modelo

Outra técnica para adquirir confiança no código é testar sobre uma implementação modelo. Podemos relacionar a nossa implementação de ordenação de lista com a função de ordenação presente na biblioteca padrão, se elas possuem o mesmo comportamento, nós ganhamos confiança que nossa função de ordenação faz o que é certo:

\-\- file: ch11/QC-basics.hs

prop\_sort\_model xs = sort xs == qsort xs

Este tipo de teste baseado em modelo é extremamente poderoso. Geralmente, desenvolvedores irão ter uma implementação de referência ou protótipo que, embora ineficiente, é correta. Isso pode então ser mantido por perto e usado para assegurar que o código de produção otimizado está de acordo com a referência. Ao construir uma grande suíte desses testes baseados em modelos e executando-os regularmente(em cada commit, por exemplo), podemos facilmente assegurar a precisão de nosso código. Grandes projetos Haskell geralmente possuem suítes de propriedades de tamanho comparável com o próprio projeto, com milhares de invariantes testadas em cada mudança, mantendo o código de acordo com a especificação e assegurando que ele se comporta como requerido.

Testando o caso de uso: especificando uma Pretty Printer

Testar as propriedades naturais de  funções individuais é um das mais básicas abordagens que guiam o desenvolvimento de grandes sistemas em Haskell. Veremos agora um cenário mais complicado: construir uma suíte de testes para a biblioteca de pretty-printing* desenvolvida em capítulos anteriores.

*N.dT.: Pretty-printing é o nome que se dá à apresentação de um conteúdo de maneira em que a estrutura da apresentação intensifica o sentido do próprio conteúdo

Gerando Dados de Teste

Lembre-se que a pretty printer é construída de acordo com o Doc, um tipo de dado algébrico que representa documentos bem-estruturados.

\-\- file: ch11/Prettify2.hs

data Doc = Empty

| Char Char

| Text String

| Line

| Concat Doc Doc

| Union Doc Doc

deriving (Show,Eq)

A biblioteca em si é implementada como um conjunto de funções que criam e transformam valores deste tipo de documento, antes de finalmente criar a sua representação completa em uma string.

QuickCheck encoraja uma abordagem para testes onde o desenvolvedor especifica invariantes que deveriam ser verdadeiras para quaisquer dados que sejam consumidos pelo código. Para testar a biblioteca de pretty-printing, então, precisaremos de uma fonte de dados de entrada. Para isso, usufruímos da pequena suíte de combinação para construção de dados randômicos que o QuickCheck provê, via a classe Arbitrary. Essa classe fornece uma função, arbitrary, que gera dados de diferentes tipos. Com ela, podemos definir nosso gerador de dados para nossos próprios tipos de dados:

\-\- file: ch11/Arbitrary.hs

class Arbitrary a where

arbitrary :: Gen a

Algo a ser notado é geradores são executados em um ambiente Gen, indicado pelo tipo. Isso é um simples monad passa-estados que é usar para esconder o estado do gerador de número randômico, que é espalhado pelo código. Examinaremos monads minuciosamente em capítulos posteriores, por agora é suficientes dizer que, como Gen é definido como um monad, nós podemos usar a sintaxe do para escrever novos geradores que acessam o código de números randômicos implícito. Na realidade, para escrever geradores para nosso próprio tipo, usamos qualquer conjunto de funções definidas na biblioteca para introduzir novos valores randômicos, para posteriormente juntá-los para construir estruturas de dados nas quais estejamos interessantes. Os tipos da funções chave são:

\-\- file: ch11/Arbitrary.hs

elements :: \[a\] -> Gen a

choose :: Random a => (a, a) -> Gen a

oneof :: \[Gen a\] -> Gen a

A função elements, por exemplo, recebe uma lista de valores e retorna um gerador de valores randômicos a partir daquela lista. (Usaremos choose e oneof depois). Com isso, podemos começar a escrever geradores para tipos de dados simples. Por exemplo, se definirmos um novo tipo de dado para a lógica ternária:

\-\- file: ch11/Arbitrary.hs

data Ternary

= Yes

| No

| Unknown

deriving (Eq,Show)

Podemos escrever uma instância de Arbitrary para o tipo Ternary ao definir uma função que escolhe um elemento da lista dos possíveis valores do tipo Ternary:

\-\- file: ch11/Arbitrary.hs

instance Arbitrary Ternary where

arbitrary = elements \[Yes, No, Unknown\]

Outra abordagem para a geração de dados é gerar valores para um dos tipos básicos de Haskell e traduzir tais valores em tipos nos quais estejamos interessados. Poderíamos ter escrito a instância de Ternary gerando valores inteiros de 0 a 2 por exemplo, usando choose, e então mapeando os valores em valores ternários:

\-\- file: ch11/Arbitrary2.hs

instance Arbitrary Ternary where

arbitrary = do

n <- choose (0, 2) :: Gen Int

return $ case n of

0 -> Yes

1 -> No

_ -> Unknown

Para simples tipos de sum, essa abordagem funciona bem, já que os inteiros são facilmente mapeáveis para os construtores do tipo de dado. Para tipos product (como as estruturas e as tuplas), precisamos de, no lugar, gerar cara componente do produto separadamente (e recursivamente para tipos aninhados), e então combinar os componentes. Por exemplo, para gerar pares de valores randômicos:

\-\- file: ch11/Arbitrary.hs

instance (Arbitrary a, Arbitrary b) => Arbitrary (a, b) where

arbitrary = do

x <- arbitrary

y <- arbitrary

return (x, y)

Vamos escrever um gerador para todas as diferentes variantes do tipo Doc. Começaremos quebrando o problema em problemas menores, inicialmente gerando construtores randômicos para cada tipo, e então, dependendo do resultado, os componentes de cada campo. Os casos mais complicados são as variantes de concatenação e união

        Primeiro, no entanto, devemos escrever uma instância para gerar caracteres randômicos – QuickCheck não tem uma instância padrão para caracteres, devido à abundância de diferentes codificações de texto que podemos querer usar para testes de caracteres. Escreveremos nossa própria instância, e, como se não nos importássemos com o conteúdo do texto do documento em si, um gerador simples de caracteres alfabéticos e pontuações será suficiente (geradores mais abrangentes são simples extensões dessa abordagem básica):

\-\- file: ch11/QC.hs

instance Arbitrary Char where

arbitrary = elements (\['A'..'Z'\] ++ \['a' .. 'z'\] ++ "~!@#$%^&*()")

Com isso, podemos agora escrever uma instância para documentos enumerando os construtores e preenchendo os campos. Escolhemos um inteiro randômico para representar qual variante do documento será gerada, e então realizar a escolha baseada no resultado. Para gerar nós de documentos de concatenação ou união, usamos recursão sobre arbitrary, deixando a inferência de tipos determinar qual instância de Arbitrary desejamos:

\-\- file: ch11/QC.hs

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

Essa foi uma abordagem bem direta, e podemos melhorá-la um pouco mais usando a função oneof, cujo tipo vimos anteriormente, para escolher entre diferentes geradores em uma lista (podemos usar também o combinador monádico, liftM, para evitar nomear resultados intermediários de cada gerador):

\-\- file: ch11/QC.hs

instance Arbitrary Doc where

arbitrary =

oneof \[ return Empty

, liftM Char arbitrary

, liftM Text arbitrary

, return Line

, liftM2 Concat arbitrary arbitrary

, liftM2 Union arbitrary arbitrary \]

Esta última versão é mais concisa – escolhendo apenas de uma lista de geradores – embora ambas as versões descrevam os mesmo dados. Podemos checar que a saída faz sentido, ao gerar uma lista de documentos randômicos (escolhemos a semente inicial do gerador pseudo-randômico como 2):

ghci> generate 10 (System.Random.mkStdGen 2) arbitrary :: \[Doc\]

\[Line,Empty,Union Empty Line,Union (Char 'R') (Concat (Union Line (Concat (Text "i@BmSu") (Char ')'))) (Union (Concat (Concat (Concat (Text "kqV!iN") Line) Line) Line) Line)),Char 'M',Text "YdwVLrQOQh"\]

Examinando a saída, vemos uma boa mistura de casos básicos e alguns documentos aninhados mais complicados. Geraremos centenas desde a cada execução de teste para que o teste seja válido. Agora podemos escrever algumas propriedades genéricas para nossas funções

Testando a Construção de Documentos

Duas das funções básicas sobre documentos são a constante de documento nulo (função nulária), empty, e a função anexar. Seus tipos são:

\-\- file: ch11/Prettify2.hs

empty :: Doc

(<>) :: Doc -> Doc -> Doc

Juntas, essas funções deveriam compor uma propriedade razoável: anexar ou prepor uma lista vazia a uma segunda lista deveria deixar a segunda lista inalterada. Podemos afirmar essa invariante como uma propriedade:

\-\- file: ch11/QC.hs

prop\_empty\_id x =

 empty <> x == x

&&

 x <> empty == x

Ao confirmar que essa propriedade é verdadeira, podemos continuar a criação de nossos testes:

ghci> quickCheck prop\_empty\_id

passed 100 tests.

Use isso para ver quais documentos foram realmente gerados (substituindo quickCheck por verboseCheck). Se examinarmos o que foi gerado, veremos uma boa mistura de casos simples e complicados. Podemos refinar a geração de dados mais além, definindo restrições sobre a proporção de dados gerados, se desejado.

        Outras funções na API são simples o suficiente para terem o seu comportamento completamente descrito por propriedades. Fazendo isso, podemos manter uma descrição externa e verificável do comportamento da função, de modo que modificações futuras não quebrarão as invariantes básicas.

\-\- file: ch11/QC.hs

prop_char c   = char c == Char c

prop_text s   = text s == if null s then Empty else Text s

prop_line     = line == Line

prop_double d = double d == text (show d)

Essas propriedades são suficientes para testar completamente a estrutura retornada pelos operadores básicos de documentos. Testar o restante da biblioteca requer mais esforço.

Usando Listas como Modelos

Funções de alta ordem são a base de programas reusáveis, e a nossa biblioteca de pretty-printing não é exceção – uma função fold customizada é usada internamente para implementar tanto concatenação quanto intercalação de separadores entre pedaços de documentos. O fold definido para documentos recebe uma lista de pedaços de documentos e os uni de acordo com uma função de combinação:

\-\- file: ch11/Prettify2.hs

fold :: (Doc -> Doc -> Doc) -> \[Doc\] -> Doc

fold f = foldr f empty

Podemos escrever testes em isolamento para instâncias específicas de fold facilmente. A concatenação horizontal de documentos, por exemplo, é fácil de ser especificada escrevendo-se uma implementação de referência sobre listas:

\-\- file: ch11/QC.hs

prop_hcat xs = hcat xs == glue xs

where

glue \[\] = empty

glue (d:ds) = d <> glue ds

Acontece uma história similar com punctuate, onde podemos modelar a inserção de pontuação com intercalação de listas (intersperse, de Data.List,é uma função que recebe um elemento e o intercala entre outros elementos da lista):

\-\- file: ch11/QC.hs

prop_punctuate s xs = punctuate s xs == intersperse s xs

Embora pareça correta, a execução revela uma falha na nossa lógica:

ghci> quickCheck prop_punctuate

Falsifiable, after 6 tests:

Empty

\[Line,Text "",Line\]

A biblioteca de pretty-printing aperfeiçoa documentos vazios redundantes, algo que o modelo de implementação não faz, logo precisaremos aumentar o nosso modelo para satisfazer a realidade. Primeiro, intercalamos a pontuação pela lista de documentos, e então eliminamos os documentos Empty espalhados pela lista, desta maneira:

\-\- file: ch11/QC.hs

prop_punctuate' s xs = punctuate s xs == combine (intersperse s xs)

where

combine \[\] = \[\]

combine \[x\] = \[x\]

combine (x:Empty:ys) = x : combine ys

combine (Empty:y:ys) = y : combine ys

combine (x:y:ys) = x \`Concat\` y : combine ys

Executando isso no GHCi, podemos confirmar o resultado. É reconfortante que o framework de testes consiga localizar falhas em nossa lógica expressa no código – exatamente o que estamos procurando:

ghci> quickCheck prop_punctuate'

passed 100 tests.

Juntando Todas as Peças

        Podemos colocar todos estes testes juntos em um único arquivo e executá-los simplesmente usando uma das funções de processamento de QuickCheck. Existem várias, inclusive ligadas a paralelismo. Entretanto, o processamento serial normalmente é bom o suficiente. Tudo o que precisamos é criar alguns parâmetros de teste, e então listas as funções que queremos testar:

\-\- file: ch11/Run.hs

import Prettify2

import Test.QuickCheck.Batch

options = TestOptions

{ no\_of\_tests = 200

, length\_of\_tests = 1

, debug_tests = False }

main = do

runTests "simple" options

\[ run prop\_empty\_id

, run prop_char

, run prop_text

, run prop_line

, run prop_double

\]

runTests "complex" options

\[ run prop_hcat

, run prop_puncutate'

\]

Aqui, estruturamos o código como um script separado e autônomo, com instancias e propriedades em seus próprios arquivos, separados do código da biblioteca. Isso é típico para projetos de biblioteca, em que testes são mantidos separados da biblioteca em si, e eles importam a biblioteca de acordo com o sistema de módulos. O script de teste pode então ser compilado e executado:

$ ghc --make Run.hs

$ ./Run

simple : .....                (1000)

complex : ..                   (400)

Um total de 1400 testes individuais foram criados, o que é reconfortante. Podemos aumentar a profundidade facilmente, mas para saber exatamente quão bem o código está sendo testado, devemos usar a ferramenta embutida de cobertura de código, HPC, que pode afirmar com precisão o que está acontecendo.

Medir a cobertura de teste com HPC

O HPC (Haskell Program Coverage) é uma extensão para que o compilador observe quais partes do código foram realmente executadas durante a execução do programa dado. Isso é útil no contexto de teste, pois nos permite observar com precisão quais as funções, ramos e expressões foram avaliadas. O resultado é um conhecimento preciso sobre o percentual de código testado que é facilmente obtido. O HPC vem com um utilitário simples para gerar gráficos úteis de cobertura do programa, tornando mais fácil para verificar os pontos fracos no conjunto de testes.

Para a obtenção de dados de cobertura de testes, tudo o que precisamos fazer é adicionar a flag -fhpc na linha de comando, ao compilar os testes :

$ ghc -fhpc Run.hs –make

Em seguida, executar os testes normalmente:

$ ./Run

simple : ..... (1000)

complex : .. (400)

Durante a execução do teste, o traço do programa é escrito em arquivos .tix e .mix no diretório em questão. Depois disso, estes arquivos são usados pela ferramenta de linha de comando, hpc, para mostras as várias estatísticas sobre o que aconteceu. A interface básica é textual. Para começar, podemos ter um resumo do código testado durante a execução usando a flag  report para hpc. Excluiremos os programas de teste(usando a flag  --exclude), para nos concentrarmos somente no código da biblioteca pretty-printer. Entrando com o seguinte código no console:

$ hpc report Run --exclude=Main --exclude=QC

18% expressions used (30/158)

0% boolean coverage (0/3)

0% guards (0/3), 3 unevaluated

100% 'if' conditions (0/0)

100% qualifiers (0/0)

23% alternatives used (8/34)

0% local declarations used (0/4)

42% top-level declarations used (9/21)

Nós vemos que, na última linha, 42% das definições de alto-nível foram executadas durante a execução do teste. Nada mal para a primeira tentativa. Conforme testamos mais e mais funções da biblioteca, estes números irão aumentar. A versão textual é útil para uma resumo rápido, mas para ver realmente o que está acontecendo, é melhor olhar para a saída marcada. Para gerar isto, iremos usar a flag  markup:

$ hpc markup Run --exclude=Main --exclude=QC

Isto irá gerar um arquivo HTML para cada arquivo de código Haskell, e alguns arquivos de índices. Carregando o arquivo hpc_index.html em um navegador, podemos ver alguns belos gráficos de cobertura de código. Veja a figura 11-1.

![](images/image2.png)

Nada mal. Clicando no módulo, vemos que o código real do programa, com marcações em negrito e amarelo para código que não foi testado e apenas em negrito para código que foi executado.

![](images/image1.png)

Esquecemos de testar a instância Monoid, por exemplo, e algumas funções mais complicadas. HPC nos ajuda a manter a suíte de testes honesta. Vamos adicionar um teste para instância de Monoid, a classe de tipos que suportam concatenação e elementos vazios:

\-\- file: ch11/QC.hs  
prop\_mempty\_id x =  
        mempty \`mappend\` x == x  
 &&

x \`mappend\` mempty == (x :: Doc)

Executamos esta propriedade no ghci, para checar que está correta:

ghci> quickCheck prop\_mempty\_id  
        00, passed 100 tests.

Podemos agora recompilar e executar os testes. É importante remover o antigo .tix arquivo primeiro, ou um erro irá ocorrer já que o HPC tenta combinar as estatísticas de execuções separadas:

$ ghc -fhpc Run.hs --make -no-recomp

$ ./Run

Hpc failure: inconsistent number of tick boxes

(perhaps remove Run.tix file?)

$ rm *.tix

$ ./Run

simple : ..... (1000)

complex : ... (600)

Outros 200 testes foram adicionados à suíte, e nossas estatísticas de cobertura subiu para 52% do código base. (veja Figura 11-3).

![](images/image0.png)

HPC garante que nós sejamos honestos em nossos testes, assim qualquer código com menos de 100% de cobertura irá ser assinalado com uma cor diferenciada. Em particular, isso garante que o programador terá de pensar sobre casos de erro, ramificações complicadas com condições obscuras, e qualquer forma de code smell. Quando combinado com um sistema de geração de testes exaustivos, como o QuickCheck, testes se tornam uma atividade gratificante, e uma parte essencial do desenvolvimento Haskell.
