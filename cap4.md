 Capítulo <function>. Programação funcional   

[Real World Haskell](http://book.realworldhaskell.org/)de Bryan O'Sullivan, Don Stewart, e John Goerzen
-------------------------------------------------------------------------------------------------------

Capítulo . Programação funcional

[Anterior](types-and-functions.html) 

 

 [Próximo](writing-a-library-working-with-json-data.html)

Capítulo . Programação funcional
--------------------------------

**Índice analítico**

Carregue todas as observações (**devagar**)

[Pensando no Haskell](#fp.thinking)

[Um framework de linha de comando simples](#fp.framework)

[Warming up: Separação das linhas de texto portavel](#fp.splitlines)

[Um programa de conversão de fim de linha](#id589136)

[Funções infix](#fp.infix)

[Trabalhar com as listas](#fp.lists)

[Manipulação de listas básica](#fp.lists.basic)

[Trabalhar segura e saudavelmente a com funções crashy](#fp.lists.safe)

[Funções parcial e total](#id590211)

[Mais manipulações de listas simples](#id590283)

[Trabalhar com sublistas](#id590714)

[Buscando listas](#fp.list.search)

[Trabalhando com muitas listas ao mesmo tempo](#id591412)

[Funções especiais de manipulação de string](#fp.lists.strings)

[Exercícios](#id591678)

[Como pensar a respeito de loops](#fp.loop)

[Recursão explícita](#fp.tailrecursion)

[Transformando cada peça de input](#id592265)

[Mapeando sobre uma lista](#id592470)

[Seleção de peças de entrada](#fp.filter)

[Computing uma resposta sobre um conjunto](#id592937)

[A fold esquerda](#fp.foldl)

[Por dobras usar folds, maps e filters?](#id593448)

[Folding da direita](#fp.foldr.filter)

[Folds esquerdos, preguiça e space leaks](#id594142)

[Exercícios](#fp.fold.exercises)

[Leitura complementar](#id594881)

[Funções (lambda) anónimos](#fp.anonymous)

[Aplicação da função parcial e currying](#fp.partialapp)

[Secções](#id596329)

[Padrões As](#fp.aspattern)

[Reutilização de código através da composição](#fp.compose)

[Use a cabeça sabiamente](#id597329)

[Dicas para escrever código legível](#fp.readability)

[Space leaks e avaliação rigorosa](#fp.seq)

[Evitar space leaks com seq](#id597540)

[Aprender a usar o seq](#id597688)

Pensando no Haskell
-------------------

Nossa aprendizagem precoce de Haskell possui dois aspectos distintos. A primeira é chegar a um acordo com a mudança de mentalidade da programação imperativa de funcionamento: temos de substituir a programação de nossos hábitos de outras línguas. Fazemos isso não porque as técnicas imperativos são ruins, mas porque, em uma linguagem funcional de outras técnicas de trabalho melhor. [?? comments](comments: show / hide)

O nosso segundo desafio é aprender a nossa maneira de contornar o Haskell bibliotecas padrão. Como em qualquer linguagem, bibliotecas de funcionar como alavanca, habilitando-nos a multiplicar a nossa solução de problemas de energia. Bibliotecas Haskell tendem a operar em um nível maior de abstração do que aqueles em muitas outras línguas. Precisaremos trabalhar um pouco difícil aprender usar a bibliotecas, mas na troca eles oferecem uma grande quantidade de poder. [?? comments](comments: show / hide)

Neste capítulo, vamos introduzir uma série de técnicas de programação funcionais. Nós vamos recorrer a exemplos de linguagens imperativas destacar a mudança no pensamento que vamos precisar fazer. Como o fazemos, nós vamos caminhar por alguns dos fundamentos da norma de bibliotecas Haskell. Nós também intermitentemente cobrir algumas línguas mais recursos no caminho. [?? comments](comments: show / hide)

Um framework de linha de comando simples
----------------------------------------

Na maioria deste capítulo, que incidirá nos com o código que tem qualquer interacção com o mundo exterior. Manter o foco no código prático, vamos começar por desenvolver uma passagem entre o nosso código “puro” eo mundo lá fora. O nosso quadro simplesmente lê o conteúdo de um arquivo, aplicar uma função para o arquivo e escreve o resultado para outro arquivo. [?? comments](comments: show / hide)

    -- arquivo: ca04/InteraçãoCom.hs
    -- Salvar esta em um arquivo de fonte, por exemplo InteraçãoCom.hs

[?? comments](comments: show / hide)

Esta é a todos nós necessitamos de escrever simples, mas completa, de arquivo de programas de processamento. Esse é um programa completos. Nós podemos compilá-lo para um executável chamado `InteraçãoCom`como se segue. [?? comments](comments: show / hide)

    sistema> 

[?? comments](comment: add)

Se executar este programa desde o reservatório ou comandos, que aceita duas nomes de arquivos: o nome da arquivo de ler, o nome e de um arquivo para escrever. [?? comments](comments: show / hide)

    sistema> 

[?? comments](comment: add)

Algumas das notação no nosso arquivo fonte é nova. O que introduz uma palavra-chave bloco de ações que podem provocar efeitos no mundo real, tais como a leitura ou a escrita de um arquivo. O operador `<-`é o equivalente de uma atribuição dentro um bloco `do`. Esta é a explicação bastante começar nós começou. Falaremos em mais detalhe muito sobre esses detalhes do notação e de I/O em geral, em [Capítulo 7, _I/O_](io.html "Capítulo 7, I/O"). [?? comments](comments: show / hide)

Quando se deseja testar uma função que não pode falar com o mundo lá fora, que simplesmente substitui o nome `id`ino código acima com o nome da função que queremos testar. Qualquer que seja nossa função faz, ele precisa ter o tipo de String->String: em outras palavras, ela deve aceitar uma string e retornam uma string. [?? comments](comments: show / hide)

Warming up: Separação das linhas de texto portavel
--------------------------------------------------

Haskell provê uma função built-in de `lines`, que deixa nós dividir uma string de texto em linha de limites. Ele retorna um lista das cadeias de caracteres com terminação de linha omitida. [?? comments](comments: show / hide)

    ghci> 

[?? comments](comment: add)

Embora de `lines`parece úteis, se baseia em nós ler um arquivo de em “modo texto” para o trabalho. Modo texto é uma característica comum a muitas linguagens: proporciona um comportamento especial quando lêem e escrevem arquivos no Windows. Quando se lê um arquivo em modo de texto, o arquivo de biblioteca I/O traduz a fim de linha de seqüência `"\r\n"`(retorno do carro seguido por nova linha) à `"\n"`(nova linha sozinho), e faz o inverso quando Escrever um arquivo. Em semelhante sistemas do Unix, o modo de texto não exerce qualquer translação. Como resultado desta diferença, se ler um arquivo de uma plataforma que estava escrito em outro o final de linha devem se tornar uma bagunça. (Ambos `readFile`e `writeFile`operar em modo texto). [?? comments](comments: show / hide)

    ghci> 

[?? comments](comment: add)

A função `lines`só divide em caracteres de nova linha, deixando retorna carro balançando nas extremidades das linhas de. Se ler um arquivo de texto gerou-Windows em um sistema Linux ou caixa Unix nós vamos arrastando carro retorna no final da cada uma delas. [?? comments](comments: show / hide)

Nós confortavelmente usado o suporte “universal newline” do Python apoio a anos: trata isso de forma transparente Unix e Windows linha que termina as convenções para nós. Gostaríamos de oferecer algo similar em Haskell. [?? comments](comments: show / hide)

Uma vez que estamos ainda cedo em nossa carreira de leitura de código Haskell, vamos discutir nossa aplicação em Haskell bastante detalhe alguns. [?? comments](comments: show / hide)

    -- arquivo: ca04/LinhasSeparadas.hs

[?? comments](comments: show / hide)

A assinatura do tipo de nossa função indica que aceita uma única corda, o conteúdo de um arquivo com alguma linha que termina convenção desconhecido. Ele retorna a lista de seqüências de caracteres, que representa cada linha do processo. [?? comments](comments: show / hide)

    -- arquivo: ca04/LinhasSeparadas.hs

[?? comments](comments: show / hide)

Antes de nos aprofundarmos em detalhes, como primeira notícia que se organizaram nosso Código. Nós apresentamos as partes importantes de código em primeiro lugar, mantendo a definição de `éSeparadorDeLinhas`depois. Porque demos a função auxiliar um nome readable, que podemos adivinhar o que ele faz ainda antes temos lido, que facilita um “fluxo” suave de lendo de código. [?? comments](comments: show / hide)

O Prelude define uma função chamada `break`que podemos usar para particionar um de lista em duas partes. É preciso uma função que seu primeiro parâmetro. Essa função deverá examinar elementos de lista, e retorna uma Boolindicar se deseja interromper a lista nesse momento. A função `break`retorna um par, que consiste no sublista consumidos antes do predicado retornado `True`(o _prefixo_), eo resto da lista (o _sufixo_). [?? comments](comments: show / hide)

    ghci> 

[?? comments](comment: add)

Uma vez que só precisa corresponder a um único transporte de retorno ou nova linha de cada vez, analisar um elemento da lista ao mesmo tempo é bom o suficiente para as necessidades. [?? comments](comments: show / hide)

A equação primeira de `linhasSeparadas`indica que, se coincidir com uma seqüência vazia, não temos nenhum trabalho ainda a fazer. [?? comments](comments: show / hide)

Na equação do segundo, aplicar primeiro `break`a nossa string de entrada. O prefixo é a substring antes de um terminador de linha, e sufixo é o restante do fio. O sufixo incluirão o terminador linha é eventualmente presentes.[?? comments](comments: show / hide)

A expressão “`prefixo :`” indica-nos que devemos adicionar o valor `prefixo`para a frente a lista das linhas. Em seguida, use uma expressão de `case`para inspecionar os sufixos, assim que nós podemos decidir o que fazer. O resultado da expressão `case`será utilizada como argumento segundo o construtor da lista `(:)`. [?? comments](comments: show / hide)

O primeiro padrão corresponde a uma seqüência que começa com um regresso transporte, seguido por uma linha nova. A variável de `resto`está ligado ao restante da cadeia. Os outros padrões são parecidos, então elas devem ser fácil de acompanhar. [?? comments](comments: show / hide)

Uma descrição em prosa de uma função Haskell não necessariamente fáceis de seguir. Podemos obter uma melhor compreensão por parte entrando **ghci**, e observar o comportamento da função em circunstâncias diferentes. [?? comments](comments: show / hide)

Começamos a separação por uma seqüência que não contém qualquer separadores de linhas. [?? comments](comments: show / hide)

    ghci> 

[?? comments](comment: add)

Aqui a nossa aplicação da `break`nunca encontra um terminador de linha, assim que o sufixo retorna vazio. [?? comments](comments: show / hide)

    ghci> 

[?? comments](comment: add)

A expressão `case`em `linhasSeparadas`como tal devem ser combinados no quarto ramo e estamos acabados. E quanto um caso um pouco mais interessante? [?? comments](comments: show / hide)

    ghci> 

[?? comments](comment: add)

Nossa primeira aplicação de `break`nos oferece um sufixo não vazio. [?? comments](comments: show / hide)

    ghci> 

[?? comments](comment: add)

Devido o sufixo começa com um regresso transporte, seguido de uma nova linha, que correspondem a primeira sucursal de a expressão `case`. Isto dá-nos `prefixo`ligado a `"foo"`, e `sufixo`ligado a `"bar"`. Nós aplicar `linhasSeparadas`recursivamente, desta vez no `"bar"`sozinho. [?? comments](comments: show / hide)

    ghci> 

[?? comments](comment: add)

O resultado é que vamos construir a lista cuja cabeça é `"foo"`e cuja cauda é `["bar"]`. [?? comments](comments: show / hide)

    ghci> 

[?? comments](comment: add)

Este tipo de experimentos com **ghci**Este tipo de experimentos com **ghci**, por isso tendem a gravação funciona mais pequenas. Esta pode ainda ajudar a legibilidade do código. [?? comments](comments: show / hide)

Este estilo de criar e de reutilização de partes pequenas, poderoso do código é uma parte fundamental da programação funcional. [?? comments](comments: show / hide)

### Um programa de conversão de fim de linha

Deixe de ligar a nossa função `linhasSeparadas`em âmbito pouco que escreveu anteriormente. Faça um cópia do arquivo de fonte `InteraçãoCom.hs`; vamos chamar o arquivo novo `LinhasAdaptadas.hs`. Adicione a função `linhasSeparadas`para o novo arquivo de origem. Desde a nossa função precisa elaborar um único String, temos que costurar a lista de linhas de volta. O Prelude fornece uma função `unlines`que concatena a lista das cadeias, acrescentando uma nova linha para o final de cada um. [?? comments](comments: show / hide)

    -- arquivo: ch04/LinhasAdaptadas.hs

[?? comments](comments: show / hide)

Se substitui a função `id`com `linhasAdaptadas`, podemos compilar um executável que irá converter um arquivo de texto para a linha materna nosso sistema termina. [?? comments](comments: show / hide)

    sistema> 

[?? comments](comment: add)

Se você é em um sistema Windows, localizar e transferir um arquivo texto criado em um sistema Unix (por exemplo [gpl-3.0.txt](http://www.gnu.org/licenses/gpl-3.0.txt)). Abrir no editor texto padrão Notepad. As linhas devem correr tudo junto, fazendo o arquivo praticamente ilegível. Process o ficheiro utilizando o **LinhasAdaptadas**mando criado e abra o arquivo de saída no Bloco de notas. As terminações de linha agora deve ser fixada acima. [?? comments](comments: show / hide)

Em semelhantes os sistemas Unix, o padrão pagers e editores esconder terminações de linha de Windows. Isto faz mais difícil de verificar se **LinhasAdaptadas**é realmente eliminá-los. Aqui estão uns poucos comandos que deve ajudar. [?? comments](comments: show / hide)

    sistema> 

[?? comments](comment: add)

Funções infix
-------------

Normalmente, quando se define ou aplicar uma função em Haskell, nós escrevemos o nome da função, seguido por seus argumentos. Esta notação é chamada de _prefixo_, porque o nome da função vem perante seus argumentos. [?? comments](comments: show / hide)

Caso uma função ou construtor necessários dois ou mais discussões, temos a opção de utilização em formulário _infixo_, onde colocar _entre_sua argumentos e segundo antes. O que nos permite usar funções como operador infixo. [?? comments](comments: show / hide)

Definir ou aplicar uma função de construtor ou o valor usando a notação infixo, nós coloque seu nome nos personagens backtick (também conhecido como backquotes). Aqui estão as definições infixo simples de uma função e um tipo. [?? comments](comments: show / hide)

    -- arquivo: ca04/Plus.hs

[?? comments](comments: show / hide)

Dado que a notação infixa é meramente uma conveniência sintático, não muda a função de comportamento um. [?? comments](comments: show / hide)

    ghci> 

[?? comments](comment: add)

A notação Infix pode frequentemente ajudar legibilidade. De exemplo Prelude define uma função, `elem`, que indicam se há um valor presente em um de lista. Se usarmos `elem`com anotação de prefixo, é bastante fácil de ler. [?? comments](comments: show / hide)

    ghci> 

[?? comments](comment: add)

Se vamos mudar a infixo registo, o código fica até mais fácil de entender. É agora claro que estamos verificando se o valor à esquerda está presente na lista da certo. [?? comments](comments: show / hide)

    ghci> 

[?? comments](comment: add)

Vemos uma expressiva melhora mais com algumas funções úteis da módulo `Data.List`. A função `isPrefixOf`diz-nos se numa lista coincide com o começo de outra. [?? comments](comments: show / hide)

    ghci> 

[?? comments](comment: add)

As funções `isInfixOf`e `isSuffixOf`corresponder qualquer lugar em um lista e em seu final, respectivamente. [?? comments](comments: show / hide)

    ghci> 

[?? comments](comment: add)

Não há e rápida regra rígida que determina quando você deveria usar infixo versus notação de prefixo, apesar de notação de prefixo é muito mais comuns. É a mais melhor escolher o que faz o seu código mais legível em uma determinada situação. [?? comments](comments: show / hide)

![[Note]](support/figs/note.png)

Cuidados com anotação familiar em um idioma desconhecido

A algumas outras linguagens de programação utilizar backticks mas, apesar das semelhanças visual, a fim de backticks em Haskell não lembram remotamente o seu significado em, por exemplo: Perl, Python, shell scripts ou Unix. [?? comments](comments: show / hide)

A única coisa legal que podemos fazer com backticks em Haskell é envolver-los ao redor do nome da função. Não pode, por exemplo usá-las para incluir a expressão complexa cujo valor é uma função. Pode ser conveniente, se pudéssemos, mas que não é como a linguagem actual. [?? comments](comments: show / hide)

Trabalhar com as listas
-----------------------

Como o pão ea manteiga da programação funcional, listas de merecer alguma atenção. O prelúdio norma define dezenas de funções para lidar com listas. Muitos destes serão ferramentas indispensáveis, por isso é importante que eles aprendam desde cedo.[?? comments](comments: show / hide)

Para melhor ou pior, esta secção vai ler um pouco como uma “lista suja” de funções. Por apresentar muitas funções de modo ao mesmo tempo? Essas funções são fáceis de aprender e absolutamente ubíqua. Se não temos essa caixa de ferramentas em nossas mãos, vamos acabar perdendo tempo reinventando funções simples que já estão presentes nas bibliotecas padrão;. Então fique com a gente como nós atravessamos a lista o esforço que você vai economizar se ser enorme.[?? comments](comments: show / hide)

O módulo `Data.List` e o casa lógica “real” de todas as funções da lista. O Prelude meramente re-exporta uma grande subconjunto das funções exportadas pela `Data.List`. Diversas funções úteis no `Data.List` _não_ são re-exportados pelo prelúdio padrão. Ao andarmos funções de lista nas seções que seguem, vamos mencionar explicitamente aqueles que estão apenas em `Data.List`.[?? comments](comments: show / hide)

    ghci> 

[?? comments](comment: add)

Como nenhuma dessas funções é complexo ou tem mais de cerca de três linhas de Haskell para escrever, vamos ser breves nas nossas descrições de cada um. De fato, uma aprendizagem útil e rápido exercício é escrever uma definição de cada função depois que você já leu sobre isso.[?? comments](comments: show / hide)

### Manipulação de listas básica

A função `length` nos informa quantos elementos estão em uma lista.[?? comments](comments: show / hide)

    ghci> 

[?? comments](comment: add)

Se você precisa determinar se uma lista está vazia, use a função `null`.[?? comments](comments: show / hide)

    ghci> 

[?? comments](comment: add)

Para acessar o primeiro elemento de uma lista, usamos a função `head`.[?? comments](comments: show / hide)

    ghci> 

[?? comments](comment: add)

O inverso, `tail`, volta tudo, _mas_ a cabeça de uma lista.[?? comments](comments: show / hide)

    ghci> 

[?? comments](comment: add)

Outra função, `last`, retorna o último elemento de uma lista.[?? comments](comments: show / hide)

    ghci> 

[?? comments](comment: add)

O inverso da `last` é `init`, que retorna uma lista de todos mas o último elemento de sua entrada.[?? comments](comments: show / hide)

    ghci> 

[?? comments](comment: add)

Várias das funções acima se comportam mal em uma lista vazia, então tome cuidado se você não souber ou não uma lista está vazia. Como se dá sua má conduta tomar?[?? comments](comments: show / hide)

    ghci> 

[?? comments](comment: add)

Tente cada uma das funções acima, no **ghci**. Quais falhar quando dada uma lista vazia?[?? comments](comments: show / hide)

### Trabalhar segura e saudavelmente a com funções crashy

Quando queremos usar uma função como a `head`, onde sabemos que poderia explodir em nós se passar em uma lista vazia, a tentação pode inicialmente ser forte para verificar o comprimento da lista antes que chamamos de `head`. Vamos construir um exemplo artificial para ilustrar o nosso ponto.[?? comments](comments: show / hide)

    -- arquivo: ca04/ListaEfficiente.hs

[?? comments](comments: show / hide)

Se nós estamos vindo de uma linguagem como Perl ou Python, isso pode parecer uma forma perfeitamente natural para escrever este ensaio. Nos bastidores, as listas de Python são matrizes, matrizes e Perl são, assim, matrizes. Então, eles necessariamente saber quanto tempo eles estão, e chamando `len(foo)` ou `scalar(@foo)` é natural coisa perfeitamente fazer. Mas como acontece com muitas outras coisas, não é uma boa idéia cegamente transplante de tal pressuposto em Haskell.[?? comments](comments: show / hide)

Nós já vimos a definição do tipo de dados algébrica lista muitas vezes, e sei que a lista não armazena seu próprio comprimento explicitamente. Assim, a única maneira de `length` pode operar é andar toda a lista.[?? comments](comments: show / hide)

Portanto, quando só se preocupam ou não uma lista é vazia, chamada `length` não é uma boa estratégia. Ele pode, potencialmente, fazer um trabalho muito mais do que nós queremos, se a lista que estamos trabalhando é finito. Desde Haskell nos permite facilmente criar listas de infinito, uma utilização descuidada de `length` pode até resultar em um loop infinito.[?? comments](comments: show / hide)

A função mais adequada para chamar aqui ao contrário é `null`, que é executado em tempo constante. Melhor ainda, usando `null` torna nosso código de indicar o imóvel da lista que realmente nos importa. Aqui estão duas maneiras de se expressar melhor `meuExemploEstúpido`. [?? comments](comments: show / hide)

    -- arquivo: ca04/ListaEfficiente.hs

[?? comments](comments: show / hide)

### Funções parcial e total

Funções que só têm valores de retorno definido para um subconjunto de entradas válidas são chamadas de funções _parciais_ (chamar `error` não se qualifica como retornar um valor!). Nós chamamos funções que retornam resultados válidos sobre os seus domínios de entrada inteira funções _totais_.[?? comments](comments: show / hide)

É sempre uma boa idéia para saber se uma função que você está usando é parcial ou total. Chamar uma função parcial, com uma entrada que não pode suportar é provavelmente a maior fonte de simples, os erros evitáveis em programas Haskell.[?? comments](comments: show / hide)

Alguns programadores Haskell ir tão longe para dar nomes de funções parciais que começam com um prefixo, como `unsafe`, para que eles não podem atirar no próprio pé acidentalmente.[?? comments](comments: show / hide)

É indiscutivelmente uma deficiência do prelúdio padrão que define um bom número funções parciais “inseguros”, como a `head`, sem oferecer equivalentes totais “seguros”.[?? comments](comments: show / hide)

### Mais manipulações de listas simples

O nome Haskell para a função “append” é `(++)`. [?? comments](comments: show / hide)

    ghci> 

[?? comments](comment: add)

A função `concat` recebe uma lista de listas, todas do mesmo tipo, e concatena-los em uma única lista.[?? comments](comments: show / hide)

    ghci> 

[?? comments](comment: add)

Ele remove um nível de aninhamento.[?? comments](comments: show / hide)

    ghci> 

[?? comments](comment: add)

A função `reverse` retorna os elementos de uma lista em ordem inversa.[?? comments](comments: show / hide)

    ghci> 

[?? comments](comment: add)

Para listas de Bool, as funções `and` e `or`, generalizar seus primos de dois argumentos`(&&)` e `(||)`, sobre as listas.[?? comments](comments: show / hide)

    ghci> 

[?? comments](comment: add)

Eles têm primos mais úteis, `all` e `any`, que operam em listas de qualquer tipo. Cada um leva um predicado como seu primeiro argumento, `all`retorna `True` se o predicado for bem-sucedido em cada elemento da lista, enquanto `any`retorna `True` se o predicado for bem-sucedido em pelo menos um elemento da lista.[?? comments](comments: show / hide)

    ghci> 

[?? comments](comment: add)

### Trabalhar com sublistas

A função `take`, de que já reuniu em [“aplicação de função”](types-and-functions.html#funcstypes.calling "“aplicação de função”"), retorna uma sublista consistindo de primeiros _k_ elementos de uma lista. Seu inverso, `drop`, quedas de _k_ elementos, desde o início da lista.[?? comments](comments: show / hide)

    ghci> 

[?? comments](comment: add)

A função `splitAt` combina as funções de `take` e `drop`, voltando um par da lista de entrada, dividido o índice determinado.[?? comments](comments: show / hide)

    ghci> 

[?? comments](comment: add)

As funções `takeWhile` e `dropWhile`levar predicados: `takeWhile` toma elementos a partir do início de uma lista tão longa quanto o predicado retornar `True`, enquanto `dropWhile` gotas elementos da lista, enquanto o predicado retornar `True`.[?? comments](comments: show / hide)

    ghci> 

[?? comments](comment: add)

Assim como `splitAt`“tuplas” os resultados de `take` e `drop`, as funções `break` (que já vimos na [seção chamada “Warming up: Separação das linhas de texto portavel”](#fp.splitlines "seção chamada “Warming up: Separação das linhas de texto portavel”")) e `span` até tupla os resultados de `takeWhile` e `dropWhile`.[?? comments](comments: show / hide)

Cada função tem um predicado; `break` consome a sua entrada enquanto o predicado falha, enquanto `span` consome enquanto seu predicado êxito.[?? comments](comments: show / hide)

    ghci> 

[?? comments](comment: add)

### Buscando listas

Como já vimos, a função `elem`indica se um valor está presente em uma lista. Ele tem uma função complementar, `notElem`.[?? comments](comments: show / hide)

    ghci> 

[?? comments](comment: add)

Para uma pesquisa mais geral, `filter` tem um predicado, e retorna todos os elementos da lista em que o predicado for bem-sucedido.[?? comments](comments: show / hide)

    ghci> 

[?? comments](comment: add)

Em `Data.List`, três predicados, `isPrefixOf`, `isInfixOf` e `isSuffixOf`, vamos testar a presença de sublistas dentro de uma grande lista. A maneira mais fácil de usá-los é usando a notação infixa.[?? comments](comments: show / hide)

A função `isPrefixOf` nos diz se o seu argumento deixou coincide com o início da sua tese direita.[?? comments](comments: show / hide)

    ghci> 

[?? comments](comment: add)

A função `isInfixOf` indica se o seu argumento de esquerda é uma sublista de seu direito.[?? comments](comments: show / hide)

    ghci> 

[?? comments](comment: add)

A operação de `isSuffixOf` não deve precisar de qualquer explicação.[?? comments](comments: show / hide)

    ghci> 

[?? comments](comment: add)

### Trabalhando com muitas listas ao mesmo tempo

A função `zip`recebe duas listas e “fecha-los” em uma única lista de pares. A lista resultante é o mesmo comprimento que o mais curto dos dois insumos.[?? comments](comments: show / hide)

    ghci> 

[?? comments](comment: add)

Mais útil é `zipWith`, que pega duas listas e aplica uma função para cada par de elementos, gerando uma lista que é do mesmo comprimento que o menor dos dois.[?? comments](comments: show / hide)

    ghci> 

[?? comments](comment: add)

O sistema de tipo de Haskell torna um desafio interessante para escrever funções que recebem número variável de argumentos\[[8](#ftn.id591518)\]. Portanto, se queremos zip três listas em conjunto, chamamos `zip3` ou `zipWith3`, e assim por diante até `zip7` e `zipWith7`.[?? comments](comments: show / hide)

### Funções especiais de manipulação de string

Nós já encontramos a função padrão `lines` em [a seção chamada “Warming up: Separação das linhas de texto portavel”](#fp.splitlines "a seção chamada “Warming up: Separação das linhas de texto portavel”"), eo seu homólogo padrão, `unlines`. Observe que `unlines` sempre coloca uma nova linha no final do seu resultado.[?? comments](comments: show / hide)

    ghci> 

[?? comments](comment: add)

A função `words` divide uma seqüência de entrada em qualquer espaço em branco. Sua contraparte, `unwords`, usa um único espaço para participar de uma lista de palavras.[?? comments](comments: show / hide)

    ghci> 

[?? comments](comment: add)

### Exercícios

**1.**

Escreva seus próprios definições “seguras” das funções de lista parcial normal, mas certifique-se que o seu nunca falham. Como dica, você pode querer considerar usando os seguintes tipos.[?? comments](comments: show / hide)

    -- arquivo: ca04/ch04.exercises.hs

[?? comments](comments: show / hide)

**2.**

Escreva uma função `splitWith` que atua de forma semelhante a `words`, mas leva um predicado e uma lista de qualquer tipo, e divide sua lista de entrada em cada elemento para o qual o predicado retornar `False`.[?? comments](comments: show / hide)

    -- arquivo: ca04/ch04.exercises.hs

[?? comments](comments: show / hide)

**3.**

Usando a estrutura de comando da [seção chamada “Um framework de linha de comando simples”](#fp.framework "seção chamada “Um framework de linha de comando simples”"), escreva um programa que imprime a primeira palavra de cada linha de sua entrada.[?? comments](comments: show / hide)

**4.**

Escreva um programa que transpõe o texto em um arquivo. Por exemplo, ele deve converter `"hello\nworld\n"` para `"hw\neo\nlr\nll\nod\n"`.[?? comments](comments: show / hide)

Como pensar a respeito de loops
-------------------------------

Diferentemente das linguagens tradicionais, Haskell não tem nem um `for` loop nem `while` loop. Se nós temos um monte de dados para processar, o que queremos usar no lugar? Existem várias respostas possíveis a esta pergunta.[?? comments](comments: show / hide)

### Recursão explícita

Uma maneira simples de fazer o salto a partir de uma linguagem que tem laços com uma que não é executado através de alguns exemplos, olhando para as diferenças. Aqui está uma função C que recebe uma string de dígitos decimais e os transforma em um inteiro.[?? comments](comments: show / hide)

int as_int(char *string){
    int acc; `/* accumulate the partial result */`
    for(acc= 0; isdigit(*string); string++)
        acc= acc\*10 + (\*string - '0');
    return acc;
}

[?? comments](comments: show / hide)

Dado que Haskell não possui construções de repetição, como devemos pensar sobre o que representa um simples pedaço bastante de código como este?[?? comments](comments: show / hide)

Nós não temos de começar por escrever um tipo de assinatura, mas ajuda a nos lembrar que estamos trabalhando.[?? comments](comments: show / hide)

    -- arquivo: ca04/IntParse.hs

[?? comments](comments: show / hide)

O código C calcula o resultado de forma incremental, uma vez que percorre a string, o código Haskell pode fazer a mesma função. No entanto, Haskell, podemos expressar o equivalente a um ciclo como um arquivo. Vamos chamar o nosso `loop` só para manter as coisas agradáveis e explícita.[?? comments](comments: show / hide)

    -- arquivo: ca04/IntParse.hs

[?? comments](comments: show / hide)

Esse primeiro parâmetro para `loop` é a variável acumulador estaremos usando. Passando em zero é equivalente a inicialização do `acc` variável em C, no início do loop.[?? comments](comments: show / hide)

Ao invés de pular em código chamas, vamos pensar sobre os dados que temos para trabalhar. Nossa String familiar é apenas um sinônimo para \[Char\], uma lista de caracteres. A maneira mais fácil para nós para obter o direito de passagem é para pensar sobre a estrutura de uma lista: é vazia ou um único elemento seguido pelo resto da lista.[?? comments](comments: show / hide)

Podemos expressar este pensamento estrutural directamente pelo padrão correspondente na lista de construtores do tipo. Muitas vezes é útil pensar sobre os casos fáceis primeiro: aqui, o que significa que vamos considerar o caso de lista vazia.[?? comments](comments: show / hide)

    -- arquivo: ca04/IntParse.hs

[?? comments](comments: show / hide)

Uma lista vazia não significa apenas “o String de entrada está vazia”; é também o caso, vamos encontrar quando percorremos todo o caminho até o fim de uma lista não-vazia para fora. Então, nós não queremos “erro” se vemos uma lista vazia. Em vez disso, devemos fazer algo sensato. Aqui, a única coisa sensata é a de terminar o ciclo, e voltar o nosso valor acumulado.[?? comments](comments: show / hide)

O outro caso temos que considerar surge quando a lista de entrada não estiver vazia. Precisamos fazer alguma coisa com o elemento atual da lista, e algo com o resto da lista.[?? comments](comments: show / hide)

    -- arquivo: ca04/IntParse.hs

[?? comments](comments: show / hide)

Calculamos um novo valor para o acumulador, e dar-lhe o nome de `acc'`. Em seguida, chamamos a função `words` divide uma seqüência de entrada em qualquer espaço em branco. Sua contraparte, `unwords`, usa um único espaço para participar de uma lista de palavras.[?? comments](comments: show / hide)

![[Note]](support/figs/note.png)

As aspas simples em nomes de variáveis

Lembre-se, uma única citação é um personagem legal para usar em um nome de variável Haskell, e é pronunciado como “prime”. Há uma expressão comum em programas Haskell envolvendo uma variável, digamos `foo` e outra variável, por exemplo `foo'`. Normalmente podemos assumir que `foo'` é de alguma forma relacionada com `foo`. É muitas vezes um novo valor para `foo`, como no nosso código acima.[?? comments](comments: show / hide)

Às vezes, vamos ver essa expressão alargado, como `foo''`. Como manter o controle do número de aspas simples tacheada no final de um nome rapidamente se torna enfadonho, o uso de mais de dois em uma fileira, felizmente, é rara. Na verdade, mesmo uma única citação pode ser fácil de se perder, o que pode levar a confusão por parte dos leitores. Talvez seja melhor pensar no uso de aspas simples como uma convenção de codificação que você deve ser capaz de reconhecer, e menos como um que você deve realmente seguir.[?? comments](comments: show / hide)

Cada vez que a função `loop` chama a si mesmo, tem um novo valor para o acumulador, e consome um elemento da lista de entrada. Eventualmente, ele vai acertar o final da lista, em que o tempo `[]` padrão irá corresponder, e as chamadas recursivas cessará.[?? comments](comments: show / hide)

Como isso funciona bem a função? Para inteiros positivos, é perfeitamente cromulent.[?? comments](comments: show / hide)

    ghci> 

[?? comments](comment: add)

Como isso funciona bem a função? Para inteiros positivos, é perfeitamente cromulent.[?? comments](comments: show / hide)

    ghci> 

[?? comments](comment: add)

Vamos adiar a fixação função nossas deficiências para [Q: 1](#fp.asInt.fix "Q: 1").[?? comments](comments: show / hide)

Porque a última coisa que `loop` faz é simplesmente chamar a si mesma, é um exemplo de uma função recursiva cauda. Há um outro idioma comum neste código, também. Pensando sobre a estrutura da lista, e manuseio e não vazio casos vazios separadamente, é um tipo de abordagem chamada de _recursão estrutural_.[?? comments](comments: show / hide)

Chamamos o caso não-recursiva (quando a lista estiver vazia) o _caso de base_(por vezes o _caso de terminação_). Vamos ver as pessoas se referem ao caso em que a função chama a si mesmo como o caso recursivo (surpresa!), Ou eles podem dar um aceno para a indução matemática e chamá-lo _caso indutivo_.[?? comments](comments: show / hide)

Como uma técnica útil, recursão estrutural não está confinada a lista, podemos usá-lo em outros tipos de dados algébricos, também. Teremos mais a dizer sobre isso mais tarde.[?? comments](comments: show / hide)

![[Note]](support/figs/note.png)

Qual é a grande coisa sobre recursão de cauda?

Em uma linguagem imperativa, um loop é executado no espaço constante. Sem laços, nós usamos cauda funções recursivas em Haskell vez. Normalmente, uma função recursiva aloca um espaço cada vez que aplica-se, por isso sabe para onde voltar.[?? comments](comments: show / hide)

Claramente, uma função recursiva estaria em uma enorme desvantagem em relação a um loop se memória alocada para cada aplicação recursiva: isso exigiria espaço linear em vez de espaço constante. No entanto, as implementações de linguagem funcional detectar usos de recursão de cauda, e transformar a cauda chamadas recursivas para executar no espaço constante, isso é chamado de _tail call optimisation_, abreviado TCO. [?? comments](comments: show / hide)

Poucas implementações linguagem imperativa realizar o TCO, que é por isso que usar qualquer tipo de estilo ambiciosa funcional em uma linguagem imperativa, muitas vezes leva à perda de memória e baixo desempenho.[?? comments](comments: show / hide)

### Transformando cada peça de input

Considere uma outra função C, `quadrado`, que quadrados cada elemento em um array.[?? comments](comments: show / hide)

void quadrado(double \*resultado, const double \*entrada, size_t comprimento){
    for(size_t no= 0; no < comprimento; no++)
        resultado\[no\]= entrada\[no\] * entrada\[no\];
}

[?? comments](comments: show / hide)

Este contém um tipo simples e comum de loop, que faz exatamente a mesma coisa a cada elemento da sua matriz de entrada. Como podemos escrever este circuito em Haskell?[?? comments](comments: show / hide)

    -- arquivo: ca04/Map.hs

[?? comments](comments: show / hide)

Nossa função `quadrado` consiste de duas equações a correspondência de padrão. O primeiro “desconstrói” o início de uma lista não-vazia, para obter a sua cabeça ea cauda. É praças o primeiro elemento, em seguida, coloca que na frente de uma nova lista, que é construída chamando `quadrado` no restante da lista vazio. A segunda equação garante que `quadrado` pára quando ela atinge o final da lista de entrada.[?? comments](comments: show / hide)

O efeito de `quadrado` é a construção de uma nova lista que é do mesmo tamanho que a sua lista de entrada, com cada elemento da lista de entrada com a sua praça substituído na lista de saída.[?? comments](comments: show / hide)

Aqui está um outro ciclo C tal, aquele que garante que cada letra em uma string é convertida em maiúsculas.[?? comments](comments: show / hide)

#include <ctype.h>

char \*maiuscula(const char \*entrada){
    char *resultado= strdup(entrada);
    
    if(resultado != NULL)
        for(size_t no= 0; resultado\[no\] != '\\0'; no++)
            resultado\[no\]= toupper(resultado\[no\]);
    return resultado;
}

[?? comments](comments: show / hide)

Vamos olhar um equivalente Haskell.[?? comments](comments: show / hide)

    -- arquivo: ca04/Map.hs

[?? comments](comments: show / hide)

Aqui, nós estamos importando a função `toUpper` do módulo padrão `Data.Char`, que contém grande quantidade de funções úteis para trabalhar com dados Char.[?? comments](comments: show / hide)

Nossa função `maiuscula` segue um padrão semelhante à nossa função anterior `quadrado`. Ele termina com uma lista vazia quando a lista de entrada está vazia, e quando a entrada não estiver vazia, ela chama `toUpper` no primeiro elemento, em seguida, constrói uma nova lista de células e que o resultado de chamar-se sobre o resto da entrada lista.[?? comments](comments: show / hide)

Estes exemplos seguem um padrão comum para escrever funções recursivas sobre listas em Haskell. O _caso base_ lida com a situação onde a nossa entrada lista está vazia. O _caso recursivo_ trata de uma lista não-vazia, que faz algo com a cabeça da lista, e se chama recursivamente na cauda.[?? comments](comments: show / hide)

### Mapeando sobre uma lista

As funções `quadrado` e `maiuscula` que nós definimos produzir novas listas, que são os mesmos comprimentos de suas listas de entrada, e não apenas uma peça de trabalho por elemento. Esse é um padrão comum que prelúdio Haskell define uma função, `map`, para torná-lo mais fácil. `map` tem uma função, e aplica a cada elemento de uma lista, retornando uma nova lista construída a partir dos resultados dessas aplicações.[?? comments](comments: show / hide)

Aqui estão as nossas funções `square` e `maiuscula` reescrito para usar `map`.[?? comments](comments: show / hide)

    -- arquivo: ca04/Map.hs

[?? comments](comments: show / hide)

Este é o nosso primeiro olhar de perto uma função que recebe outra função como argumento. Podemos aprender muito sobre o `map` simplesmente inspecionando seu tipo.[?? comments](comments: show / hide)

    ghci> 

[?? comments](comment: add)

A assinatura nos diz que `map` tem dois argumentos. A primeira é uma função que assume um valor de um tipo, `a`, e retorna um valor de outro tipo, `b`.[?? comments](comments: show / hide)

Desde `map` tem uma função como argumento, nós nos referimos a ela como uma função _higher-order_ (Apesar do nome, não há nada de misterioso sobre funções de ordem superior, é apenas um termo para funções que recebem outras funções como argumentos, ou funções de retorno.). [?? comments](comments: show / hide)

Desde `map` resumos o padrão comum para as nossas funções `square` e `maiuscula` para que possamos reutilizá-lo com menos clichê, podemos olhar para o que essas funções têm em comum e descobrir como implementá-lo nós mesmos.[?? comments](comments: show / hide)

    -- arquivo: ca04/Map.hs

[?? comments](comments: show / hide)

![[Note]](support/figs/note.png)

Quais são esses wild cards que fazem lá?

Se você é novo em programação funcional, as razões para os padrões de correspondência de certas maneiras, nem sempre são óbvias. Por exemplo, na definição de `meuMap` acima, a primeira equação liga a função que está mapeando a variável `f`, mas o segundo usa cartões selvagens para ambos os parâmetros. O acontecendo estabele que?[?? comments](comments: show / hide)

Nós usamos um wild card no lugar de `f` para indicar que não estamos chamando a função `f` no lado direito da equação. E sobre a lista de parâmetros? O tipo de lista tem dois construtores. Nós já encontrados no construtor não vazia na primeira equação que define `myMap`. Por eliminação, o construtor da segunda equação é necessariamente o construtor lista vazia, então não há necessidade de realizar um jogo para ver o que realmente é o seu valor.[?? comments](comments: show / hide)

Por uma questão de estilo, é bom para usar wild cards para o bem conhecido tipos simples, como listas e Maybe. Para obter mais ou menos complicados tipos familiares, pode ser mais seguro e mais legível o nome construtores explicitamente.[?? comments](comments: show / hide)

Procuramos a nossa função `meuMap` para nos dar alguma garantia de que ele se comporta de forma semelhante ao `map`padrão.[?? comments](comments: show / hide)

    ghci> 

[?? comments](comment: add)

Este padrão de manchas um idioma repetida, então abstraí-lo para que possamos reutilizar (e escrever menos!) De código, é um aspecto comum de programação Haskell. Enquanto a abstração não é exclusivo para Haskell, funções de ordem superior tornam extremamente fácil.[?? comments](comments: show / hide)

### Seleção de peças de entrada

Outra operação comum em uma seqüência de dados é um pente fino nele para os elementos que satisfaçam algum critério. Aqui está uma função que percorre uma lista de números e retorna aqueles que são estranhos. O nosso código tem um caso recursivo que é um pouco mais complexo do que nossas funções anteriores: ele só coloca um número na lista, ele retorna se o número for ímpar. Usando um guarda expressa muito bem isso.[?? comments](comments: show / hide)

    -- arquivo: ca04/Filter.hs

[?? comments](comments: show / hide)

Vamos ver isso em ação.[?? comments](comments: show / hide)

    ghci> 

[?? comments](comment: add)

Mais uma vez, essa expressão é tão comum que o Prelude define uma função, `filter`, que já introduziu. Ele elimina a necessidade de código clichê para recurse sobre a lista.[?? comments](comments: show / hide)

    ghci> 

[?? comments](comment: add)

A função `filter` tem um predicado e aplica a cada elemento em sua lista de entrada, retornando uma lista de apenas aqueles para os quais o predicado avaliar para `True`. Nós iremos rever `filter` novamente em breve, na [seção chamada “Folding da direita”](#fp.foldr.filter "seção chamada “Folding da direita”").[?? comments](comments: show / hide)

### Computing uma resposta sobre um conjunto

Outra coisa comum de se fazer com uma coleção é reduzi-lo a um único valor. Um exemplo simples disso é somar os valores de uma lista.[?? comments](comments: show / hide)

    -- arquivo: ca04/Soma.hs

[?? comments](comments: show / hide)

Nossa função `ajudante` é cauda recursiva, e usa um parâmetro acumulador, `acc`, para segurar a soma das correntes parciais da lista. Como já vimos com `asInt`, esta é uma forma “natural” para representar um loop em uma linguagem puramente funcional.[?? comments](comments: show / hide)

Para algo um pouco mais complicado, vamos dar uma olhada na soma de verificação Adler-32. Este é um algoritmo de soma de verificação popular, que concatena duas somas de 16 bits em um soma de verificação de 32 bits único. A primeira verificação é a soma de todos os bytes de entrada, mais um. A segunda é a soma de todos os valores intermediários da soma primeiro. Em cada caso, as somas são calculadas modulo 65521. Aqui está uma simples, a aplicação Java unoptimised. (É seguro ignorar isso se você não ler Java.)[?? comments](comments: show / hide)

public class Adler32 {
    private static final int base= 65521;

    public static int compute(byte\[\] data, int offset, int length) {
    int a= 1, b= 0;
    for (int i= offset; i < offset+length; i++) {
        a= (a+ (data\[i\] & 0xff)) % base;
        b= (a+b) % base;
    }
    return (b << 16) | a;
    }
}

[?? comments](comments: show / hide)

Apesar de Adler-32 é uma soma simples, esse código não é muito fácil de ler por conta do bit girando envolvidos. Podemos fazer melhor com uma implementação de Haskell?[?? comments](comments: show / hide)

    -- arquivo: ca04/Adler32.hs

[?? comments](comments: show / hide)

Este código não é exatamente fácil de seguir do que o código Java, mas vamos olhar o que está acontecendo. Primeiro de tudo, nós introduzimos algumas novas funções. A função `shiftL` implementa um deslocamento lógico à esquerda; `(.&.)` fornece bit a bit “e”; e prevê `(.|.)` bit a bit “ou”.[?? comments](comments: show / hide)

Mais uma vez, a nossa função `ajudante` é recursiva cauda. Nós viramos as duas variáveis, atualizado em cada iteração do loop em Java em parâmetros acumulador. Quando o nosso recursão termina no final da lista de entrada, calculamos nosso soma de verificação e devolvê-lo.[?? comments](comments: show / hide)

Se dermos um passo para trás, podemos reestruturar nossas Haskell adler32 para ser mais semelhante a nossa função `meuSoma` anterior. Em vez de dois parâmetros acumulador, pode-se usar um par como o acumulador.[?? comments](comments: show / hide)

    -- arquivo: ca04/Adler32.hs

[?? comments](comments: show / hide)

Por que nós queremos fazer essa mudança, aparentemente sem sentido estrutural? Porque, como já vimos com `map` e `filter`, podemos extrair o comportamento comum compartilhado por `meuSoma` e `adler32_2` em uma função de ordem superior. Podemos descrever esse comportamento como “fazer alguma coisa para cada elemento de uma lista, atualizando um acumulador em que estamos, e retornando o acumulador quando nós somos feitos”.[?? comments](comments: show / hide)

Este tipo de função é chamado de _fold_, porque ela “dobra” de uma lista. Existem dois tipos de fold sobre listas, `foldl` para dobrar à esquerda (no início) e `foldr` para dobrar a partir da direita (o fim).[?? comments](comments: show / hide)

### A fold esquerda

Aqui está a definição de `foldl`.[?? comments](comments: show / hide)

    -- arquivo: ca04/Fold.hs

[?? comments](comments: show / hide)

A função `foldl` leva um função de “passo”, um valor inicial para o acumulador e uma lista. O “passo” leva um acumulador e um elemento da lista, e retorna um valor acumulador novo. Todo `foldl` é chamar o “passo” no acumulador atual e um elemento da lista, e passa o valor do acumulador novo para si mesmo recursivamente para consumir o restante da lista.[?? comments](comments: show / hide)

Referimo-nos a `foldl` como a “fold esquerda” porque consome a lista da esquerda (a cabeça) para a direita.[?? comments](comments: show / hide)

Aqui está uma regravação de `meuSoma` usando `foldl`.[?? comments](comments: show / hide)

    -- arquivo: ca04/Soma.hs

[?? comments](comments: show / hide)

Essa função local `passo` apenas soma dois números, então vamos simplesmente usar o operador de adição ao invés, e eliminar a cláusula desnecessária `where`clause. [?? comments](comments: show / hide)

    -- arquivo: ca04/Soma.hs

[?? comments](comments: show / hide)

Observe como muito mais simples deste código é que o nosso `meuSoma` original? Não estamos mais usando recursão explícita, porque `foldl` cuida disso para nós. Nós simplificamos o nosso problema para baixo a duas coisas: que o valor inicial do acumulador deve ser (o segundo parâmetro para `foldl`), e como atualizar o acumulador (a função `(+)`). Como um bônus adicional, o nosso código é agora mais curto, também, o que torna mais fácil de entender.[?? comments](comments: show / hide)

Vamos ter um olhar mais profundo que `foldl` está fazendo aqui, manualmente, escrevendo cada etapa em sua avaliação, quando chamamos `somaFina [1,2,3]`. [?? comments](comments: show / hide)

    -- arquivo: ca04/Fold.hs

[?? comments](comments: show / hide)

Podemos reescrever `adler32_2` usando `foldl` deixar-nos concentrar nos detalhes que são importantes.[?? comments](comments: show / hide)

    -- arquivo: ca04/Adler32.hs

[?? comments](comments: show / hide)

Aqui, o nosso acumulador é um par, assim que o resultado de `foldl` será, também. Puxamos o acumulador final distante quando retorna `foldl` e-bit mexer-lo em uma soma de verificação “apropriada”.[?? comments](comments: show / hide)

### Por dobras usar folds, maps e filters?

Uma rápida olhada revela que `adler32_foldl` não é realmente menor do que qualquer `adler32_2`. Por que devemos usar uma dobra neste caso? A vantagem reside no fato de que as dobras são extremamente comuns em Haskell, e eles têm um comportamento regular e previsível.[?? comments](comments: show / hide)

Isso significa que um leitor com um pouco de experiência terão um tempo mais fácil o entendimento a utilização de uma prega que o código que usa recursão explícita. A dobra não vai produzir nenhuma surpresa, mas o comportamento de uma função que recursivamente explicitamente não é imediatamente óbvio. recursão explícita nos obriga a ler atentamente para entender exatamente o que está acontecendo.[?? comments](comments: show / hide)

Esta linha de raciocínio se aplica a outras funções de biblioteca de ordem superior, incluindo aqueles que já vimos, `map` e `filter`. Porque eles são bibliotecas de funções com comportamentos bem definidos, só precisamos saber o que eles fazem uma vez, e nós vamos ter uma vantagem quando nós precisamos de compreender qualquer código que usa-los. Estas melhorias na legibilidade também transitar para escrever código. Assim que começar a pensar com funções de ordem superior em mente, vamos produzir um código conciso mais rapidamente.[?? comments](comments: show / hide)

### Folding da direita

A contrapartida `foldl` é `foldr`, a que dobra a partir da direita de uma lista.[?? comments](comments: show / hide)

    -- arquivo: ca04/Fold.hs

[?? comments](comments: show / hide)

Vamos seguir o mesmo processo de avaliação manual com `foldr (+) 0 [1,2,3]` como fizemos com `somaFina` na [seção chamada “A fold esquerda”](#fp.foldl "seção chamada “A fold esquerda”"). [?? comments](comments: show / hide)

    -- arquivo: ca04/Fold.hs

[?? comments](comments: show / hide)

A diferença entre `foldl` e `foldr`deve ser claro de olhar para onde os parênteses e os “lista vazia” elementos aparecem. Com `foldl`, o elemento da lista é vazia à esquerda, e todo o grupo parênteses à esquerda. Com `foldr`, o valor `zero` é à direita, eo grupo de parênteses para a direita.[?? comments](comments: show / hide)

Há uma explicação intuitiva linda `foldr` de obras como: ele substitui a lista vazia com o valor `zero`, e cada construtor na lista com uma aplicação da função de passo.[?? comments](comments: show / hide)

    -- arquivo: ca04/Fold.hs

[?? comments](comments: show / hide)

À primeira vista, `foldr` pode parecer menos úteis do que `foldl`: o uso que é uma função que se dobra a partir da direita? Mas considere a função `filter` do Prelude, que a última vez que encontrou na [seção chamada “Seleção de peças de entrada”](#fp.filter "seção chamada “Seleção de peças de entrada”"). Se escrevermos `filter` usando recursão explícita, será algo parecido com isso.[?? comments](comments: show / hide)

    -- arquivo: ca04/Fold.hs

[?? comments](comments: show / hide)

Talvez de forma surpreendente, no entanto, pode-se escrever `filter` como um fold, usando `foldr`.[?? comments](comments: show / hide)

    -- arquivo: ca04/Fold.hs

[?? comments](comments: show / hide)

Este é o tipo de definição que poderia nos causar uma dor de cabeça, por isso vamos examiná-lo em um pouco de profundidade. Como `foldl`, `foldr` tem uma função e um caso-base (o que fazer quando a lista de entrada está vazia) como argumentos. Da leitura do tipo de `filter`, nós sabemos que a nossa função `meuFilter` deve retornar uma lista do mesmo tipo que ele consome, então o caso de base deve ser uma lista desse tipo e, a função de auxiliar `passo` deve retornar uma lista.[?? comments](comments: show / hide)

Como sabemos que `foldr` calls `passo` convida um elemento da lista de entrada de cada vez, com o acumulador como seu segundo argumento, o `passo` deve ser muito simples. Se o predicado retorna `True`, ele empurra esse elemento para a lista acumulados, caso contrário, ele sai da lista intocada.[?? comments](comments: show / hide)

A classe de funções que podemos expressar utilizando `foldr` é chamada _recursiva primitiva_. Um número surpreendentemente grande de funções de manipulação de lista são recursivas primitivas. Por exemplo, aqui está a `map` escrita em termos de `foldr`.[?? comments](comments: show / hide)

    -- arquivo: ca04/Fold.hs

[?? comments](comments: show / hide)

Na verdade, podemos até escrever `foldl` usando `foldr`![?? comments](comments: show / hide)

    -- arquivo: ca04/Fold.hs

[?? comments](comments: show / hide)

![[Tip]](support/figs/tip.png)

Compreender foldl em termos de foldr

Se você quiser definir-se um desafio contínuo, tente seguir a definição acima de `foldl` usando `foldr`. Esteja avisado: esta não é trivial! Você pode querer ter as seguintes ferramentas na mão: algumas pílulas de dor de cabeça e um copo de água, **ghci**(para que você possa descobrir o que faz a função `id`), e um lápis e papel.[?? comments](comments: show / hide)

Você vai querer seguir o mesmo processo de avaliação manual como descrito acima para ver o que `foldl` e `foldr` estavam realmente fazendo. Se você ficar preso, você pode encontrar a tarefa mais fácil, depois de ler a [seção chamada “Aplicação da função parcial e currying”](#fp.partialapp "seção chamada “Aplicação da função parcial e currying”").[?? comments](comments: show / hide)

Voltando à nossa explicação anterior `foldr` intuitiva do que faz, uma outra maneira útil de pensar sobre isso é que ele _transforma_ sua entrada de lista. Os dois primeiros argumentos são “o que fazer com cada elemento da cauda da cabeça / da lista”, e “o que para substituir o final da lista”.[?? comments](comments: show / hide)

A “identidade” transformação com `foldr`assim substitui a lista vazia com ela mesma, e aplica-se o construtor de lista para cada cabeça / cauda par:x[?? comments](comments: show / hide)

    -- arquivo: ca04/Fold.hs

[?? comments](comments: show / hide)

Ela transforma uma lista em uma cópia de si mesmo.[?? comments](comments: show / hide)

    ghci> 

[?? comments](comment: add)

Se `foldr` substitui o fim de uma lista com algum outro valor, isto dá-nos uma outra maneira de olhar para afunção Haskell de acréscimo das listas, `(++)`.[?? comments](comments: show / hide)

    ghci> 

[?? comments](comment: add)

Tudo o que temos de fazer para anexar uma lista para outra é substituir essa lista segundo para o fim da nossa primeira lista.[?? comments](comments: show / hide)

    -- arquivo: ca04/Fold.hs

[?? comments](comments: show / hide)

Vamos tentar fazer isso.[?? comments](comments: show / hide)

    ghci> 

[?? comments](comment: add)

Aqui, podemos substituir cada construtor lista com outro construtor da lista, mas substituir a lista vazia com a lista que deseja acrescentar sobre o fim da nossa primeira lista.[?? comments](comments: show / hide)

Como o nosso tratamento prolongado das pregas devem indicar, a função `foldr` é quase tão importante membro da nossa caixa de ferramentas de programação lista das funções mais básicas lista vimos na [seção chamada “Trabalhar com as listas”](#fp.lists "seção chamada “Trabalhar com as listas”"). Pode consumir e produzir uma lista de forma incremental, o que o torna útil para a gravação de dados preguiçoso código de processamento.[?? comments](comments: show / hide)

### Folds esquerdos, preguiça e space leaks

Para manter o nosso simples discussão inicial, usamos `foldl` durante a maior parte desta seção. Isso é conveniente para o teste, mas nunca iremos usar `foldl` na prática.[?? comments](comments: show / hide)

A razão tem a ver com a avaliação não-estrita Haskell. Se aplicarmos `foldl (+) [1,2,3]`, que avalia a expressão `(((0 + 1) + 2) + 3)`. Podemos ver isso acontecer se rever a forma como a função é expandida.[?? comments](comments: show / hide)

    -- arquivo: ca04/Fold.hs

[?? comments](comments: show / hide)

A expressão final não será avaliada a `6` até que seu valor é exigido. Antes que seja avaliada, ele deve ser armazenado como uma conversão. Não surpreendentemente, uma conversão é mais caro do que guardar um número único, e os mais complexos a expressão thunked, o espaço mais precisa. Para algo mais barata, como aritmética, thunking um expressão é computacionalmente mais caro do que avaliá-lo imediatamente. Temos, assim, acabam pagando tanto no espaço quanto no tempo.[?? comments](comments: show / hide)

Quando GHC está avaliando uma expressão thunked, ele usa uma pilha interna para isso. Como uma expressão thunked poderia ser infinitamente grande, o GHC coloca um limite fixo sobre o tamanho máximo da pilha. Graças a esse limite, podemos tentar uma expressão de grande thunked em **ghci** sem precisar se preocupar que ele possa consumir toda a memória.[?? comments](comments: show / hide)

    ghci> 

[?? comments](comment: add)

De olhar para a expansão acima, podemos supor que este cria uma conversão que consiste em 1.000 inteiros e 999 pedidos de `(+)`. Isso é um monte de memória e esforço para representar um único número! Com uma expressão maior, embora o tamanho ainda é modesta, os resultados são mais dramáticos.[?? comments](comments: show / hide)

    ghci> 

[?? comments](comment: add)

Em expressões pequenas, `foldl` irá funcionar corretamente, mas lentamente, devido à sobrecarga thunking em que incorre. Nós nos referimos a este thunking invisível como um _space leak_(vazamento de espaço), porque o nosso código está funcionando normalmente, mas com muito mais memória do que deveria.[?? comments](comments: show / hide)

Em expressões maiores, código com um vazamento de espaço simplesmente falham, como acima. Um space leak com `foldl` é um obstáculo clássico para novos programadores Haskell. Felizmente, isso é fácil de evitar.[?? comments](comments: show / hide)

O módulo `Data.List` define uma função chamada `foldl'` que é semelhante ao `foldl`, mas não construir thunks. A diferença de comportamento entre os dois é óbvia.[?? comments](comments: show / hide)

    ghci> 

[?? comments](comment: add)

Devido ao comportamento de thunking de `foldl`, é prudente evitar essa função em programas reais: mesmo que não falham completamente, será desnecessariamente ineficiente. Em vez disso, importa `Data.List` e utilisa `foldl'`.[?? comments](comments: show / hide)

### Exercícios

**1.**

Use uma dobra (escolhendo a tampa adequada fará seu código muito mais simples) para reescrever e melhorar a função `asInt` da [secção chamada “Recursão explícita”](#fp.tailrecursion "secção chamada “Recursão explícita”").[?? comments](comments: show / hide)

    -- arquivo: ca04/ch04.exercises.hs

[?? comments](comments: show / hide)

Sua função deve se comportar como se segue.[?? comments](comments: show / hide)

    ghci> 

[?? comments](comment: add)

Estenda a sua função para tratar os seguintes tipos de condições excepcionais chamando `error`.[?? comments](comments: show / hide)

    ghci> 

[?? comments](comment: add)

**2.**

A função `asInt_fold` usa `error`, por isso seus chamadores não pode manipular erros. Reescrevê-lo para corrigir esse problema.[?? comments](comments: show / hide)

    -- arquivo: ca04/ch04.exercises.hs

[?? comments](comments: show / hide)

    ghci> 

[?? comments](comment: add)

**3.**

A função Prelude `concat`concatena uma lista de listas em uma única lista, e digite o seguinte.[?? comments](comments: show / hide)

    -- arquivo: ca04/ch04.exercises.hs

[?? comments](comments: show / hide)

Escreva a sua própria definição de `concat` usando `foldr`. [?? comments](comments: show / hide)

**4.**

Escreva a sua própria definição da função padrão `takeWhile`, primeiro usando recursão explícita, então `foldr`.[?? comments](comments: show / hide)

**5.**

O módulo `Data.List` define uma função, `groupBy`, que tem o seguinte tipo.[?? comments](comments: show / hide)

    -- arquivo: ca04/ch04.exercises.hs

[?? comments](comments: show / hide)

Use **ghci** para carregar o módulo `Data.List` e descobrir o que `groupBy` faz, em seguida, escrever sua própria implementação usando uma fold.[?? comments](comments: show / hide)

**6.**

Quantas das seguintes funções Prelude pode-se reescrever usando dobras lista?[?? comments](comments: show / hide)

*   `any`[?? comments](comments: show / hide)
    
*   `cycle`[?? comments](comments: show / hide)
    
*   `words`[?? comments](comments: show / hide)
    
*   `unlines`[?? comments](comments: show / hide)
    

Para essas funções, onde você pode usar tanto `foldl'` ou `foldr`, que é mais adequado em cada caso?[?? comments](comments: show / hide)

### Leitura complementar

O artigo \[[Hutton99](bibliography.html#bib.hutton99 "[Hutton99]")\] é um excelente e profundo tutorial pregas cobertura. Ele inclui muitos exemplos de como usar técnicas simples e sistemática de cálculo para transformar funções que usam recursão explícita em folds.[?? comments](comments: show / hide)

Funções (lambda) anónimos
-------------------------

Em muitas das definições de funções que vimos até agora, nós escrevemos funções de auxiliar de curta duração.[?? comments](comments: show / hide)

    -- arquivo: ca04/Parcial.hs

[?? comments](comments: show / hide)

Haskell lets us write completely anonymous functions, which we can use to avoid the need to give names to our helper functions. Anonymous functions are often called “lambda” functions, in a nod to their heritage in the lambda calculus. We introduce an anonymous function with a backslash character, `\`, pronounced _lambda_\[[9](#ftn.id594951)\]. This is followed by the function's arguments (which can include patterns), then an arrow `->`to introduce the function's body. [?? comments](comments: show / hide)

Lambdas are most easily illustrated by example. Here's a rewrite of `isInAny`using an anonymous function. [?? comments](comments: show / hide)

    -- arquivo: ca04/Parcial.hs

[?? comments](comments: show / hide)

Nós colocaremos o lambda parênteses aqui para que Haskell pode dizer onde o corpo da função termina.[?? comments](comments: show / hide)

Funções anónimos se comportar de forma idêntica em todos os aspectos das funções que têm nomes, mas Haskell coloca algumas restrições importantes sobre como podemos defini-los. O mais importante, enquanto nós podemos escrever uma função normal usando várias cláusulas contendo diferentes modelos e os guardas, um lambda pode ter apenas uma única cláusula na sua definição.[?? comments](comments: show / hide)

A limitação a uma única cláusula restringe como podemos usar os padrões na definição de uma lambda. Vamos escrever uma função geralmente normal, com várias cláusulas para cobrir possibilidades diferentes padrões de correspondência.[?? comments](comments: show / hide)

    -- arquivo: ca04/Lambda.hs

[?? comments](comments: show / hide)

Mas como não podemos escrever várias cláusulas para definir uma lambda, devemos estar certos de que qualquer padrão que usamos fósforo.[?? comments](comments: show / hide)

    -- arquivo: ca04/Lambda.hs

[?? comments](comments: show / hide)

Esta definição de `headInseguro` vai explodir em nossas faces se chamá-lo com um valor em que a correspondência de padrão falhar.[?? comments](comments: show / hide)

    ghci> 

[?? comments](comment: add)

A definição typechecks, assim ele vai compilar, então o erro irá ocorrer durante a execução. A moral desta história é que ter cuidado em como usar padrões para definir uma função anônima: certifique-se de seus padrões não pode falhar![?? comments](comments: show / hide)

Outra coisa a notar sobre as funções `isInAny` e `isInAny2` que mostrei acima é que a primeira versão, usando uma função auxiliar que tem um nome, é um pouco mais fácil de ler que a versão que se estatela uma função anônima para o meio. A função de auxiliar nomeado não perturbar o “fluxo” de a função na qual ele é usado, eo nome escolhido criteriosamente nos dá um pouco de informação sobre o que a função é esperado.[?? comments](comments: show / hide)

Em contraste, quando corremos em um lambda no meio de um corpo da função, temos de trocar as marchas e ler a sua definição bastante cuidado para entender o que ele faz. Para ajudar com a legibilidade e facilidade de manutenção, então, tendemos a evitar lambdas em muitas situações onde poderíamos utilizá-los para cortar alguns personagens de uma definição de função. Muitas vezes, nós vamos usar uma função parcialmente aplicado em vez disso, resultando em um código mais claro e legível do que qualquer um lambda ou uma função explícita. Não sei o que uma função parcialmente aplicado é ainda? Leia mais![?? comments](comments: show / hide)

Nós não pretendemos estas advertências para sugerir que lambdas são inúteis, mas apenas que devemos estar atentos às possíveis armadilhas quando estamos pensando em utilizá-los. Nos capítulos seguintes, veremos que são muitas vezes de valor inestimável como “cola”.[?? comments](comments: show / hide)

Aplicação da função parcial e currying
--------------------------------------

Você pode se perguntar por que a seta `->` é usado para o que parece ser a dois propósitos na assinatura de um tipo de função.[?? comments](comments: show / hide)

    ghci> 

[?? comments](comment: add)

Parece que o `->` é separar os argumentos para `dropWhile` umas das outras, mas que também separa os argumentos do tipo de retorno. Mas, na verdade `->` tem apenas um significado: ele denota uma função que recebe um argumento do tipo à esquerda, e retorna um valor do tipo do lado direito.[?? comments](comments: show / hide)

A implicação aqui é muito importante: em Haskell, _todas as funções de tomar apenas um argumento_. Quando `dropWhile` _parece_ como uma função que recebe dois argumentos, é realmente uma função de um argumento, que retorna uma função que recebe um argumento. Aqui está uma expressão perfeitamente válida Haskell.[?? comments](comments: show / hide)

    ghci> 

[?? comments](comment: add)

Bem, _isso_ parece útil. O valor `dropWhile isSpace` é uma função que retira líder espaço em branco de uma string. Como isso é útil? Como exemplo, podemos usá-lo como um argumento para uma função de ordem superior.[?? comments](comments: show / hide)

    ghci> 

[?? comments](comment: add)

Toda vez que nós fornecemos um argumento para uma função, nós podemos “cortar” um elemento fora da parte dianteira de sua assinatura tipo. Vamos tomar como exemplo `zip3` para ver o que queremos dizer, esta é uma função que fecha três listas em uma lista de três tuplas.[?? comments](comments: show / hide)

    ghci> 

[?? comments](comment: add)

Se aplicarmos `zip3` com apenas um argumento, temos uma função que aceita dois argumentos. Não importa o que nós fornecemos argumentos para esta função compostos, seu primeiro argumento será sempre o valor fixo que especificamos.[?? comments](comments: show / hide)

    ghci> 

[?? comments](comment: add)

Quando passamos menos argumentos para uma função que a função pode aceitar, nós chamamos isso de _aplicação parcial_ da função: estamos aplicando a função a que apenas alguns de seus argumentos.[?? comments](comments: show / hide)

No exemplo acima, temos uma função aplicada parcialmente, `zip3 "foo"`, e uma nova função, `zip3foo`. Podemos ver que as assinaturas do tipo dois e seu comportamento são idênticos.[?? comments](comments: show / hide)

Isto aplica-se tão bem se fixar dois argumentos, dando-nos uma função de apenas um argumento.[?? comments](comments: show / hide)

    ghci> 

[?? comments](comment: add)

Aplicação parcial de função nos permite evitar a criação de funções descartáveis cansativo. Muitas vezes é mais útil para este propósito que as funções anônimas que introduzimos na [seção chamada “Funções (lambda) anónimos”](#fp.anonymous "seção chamada “Funções (lambda) anónimos”"). Olhando para trás, a função `isInAny` nós definimos lá, aqui está como nós usaríamos uma função parcialmente aplicado em vez de uma função auxiliar chamada ou uma lambda.[?? comments](comments: show / hide)

    -- arquivo: ca04/Parcial.hs

[?? comments](comments: show / hide)

Aqui, a expressão `isInfixOf needle` é a função aplicada parcialmente. Nós estamos tomando a função `isInfixOf`, e “consertar” seu primeiro argumento a ser a variável de `needle` de nossa lista de parâmetros. Isso nos dá uma função parcialmente aplicada que tem exatamente o mesmo tipo de comportamento e como o ajudante e lambda em nossas definições anteriores.[?? comments](comments: show / hide)

Aplicação de função parcial é chamado _currying_, após o lógico Haskell Curry (para quem a linguagem Haskell é chamado).[?? comments](comments: show / hide)

Como outro exemplo de currying em uso, vamos voltar para a função lista-resumo que escrevi na [seção chamada “A fold esquerda”](#fp.foldl "seção chamada “A fold esquerda”").[?? comments](comments: show / hide)

    -- arquivo: ca04/Soma.hs

[?? comments](comments: show / hide)

Nós não precisamos de aplicar plenamente `foldl`, podemos omitir a lista de `xs` tanto a lista de parâmetros e os parâmetros para `foldl`, e nós vamos acabar com uma função mais compacto que tem o mesmo tipo.[?? comments](comments: show / hide)

    -- arquivo: ca04/Soma.hs

[?? comments](comments: show / hide)

### Secções

Haskell fornece um atalho útil para notação vamos escrever uma função parcialmente aplicadas em estilo infixo. Se colocar um operador em parênteses, nós podemos fornecer o seu argumento a esquerda ou direita dentro dos parênteses para obter uma função aplicada parcialmente. Este tipo de aplicação parcial é chamado de _section_.[?? comments](comments: show / hide)

    ghci> 

[?? comments](comment: add)

Se nos fornecer o argumento à esquerda dentro da seção, chamando a função resultante com um material argumento argumento do lado direito do operador. E vice-versa.[?? comments](comments: show / hide)

Lembre-se que nós podemos envolver um nome de função em backquotes usá-lo como um operador infixo. Isto nos permite usar seções com funções.[?? comments](comments: show / hide)

    ghci> 

[?? comments](comment: add)

A definição acima fixa o segundo argumento de `elem` dando-nos uma função que verifica se seu argumento for uma letra minúscula.[?? comments](comments: show / hide)

    ghci> 

[?? comments](comment: add)

Usando isso como um argumento para `all`, temos uma função que verifica uma seqüência inteira para ver se está tudo em minúsculas.[?? comments](comments: show / hide)

    ghci> 

[?? comments](comment: add)

Se usarmos esse estilo, podemos melhorar ainda mais a leitura de nossa função `isInAny3` anterior.[?? comments](comments: show / hide)

    -- arquivo: ca04/Parcial.hs

[?? comments](comments: show / hide)

Padrões As
----------

A função Haskell `tails`, no módulo `Data.List`, generaliza a função `tail` foi introduzida recentemente. Em vez de retornar uma “cauda” da lista, ele retorna _todos_ eles.[?? comments](comments: show / hide)

    ghci> 

[?? comments](comment: add)

Cada uma dessas cadeias é um _sufixo_ de String inicial, para `tails` produz uma lista de todos os sufixos, além de uma lista vazia extra no final. Ela produz sempre que a lista extra vazio, mesmo quando sua lista de entrada está vazia.[?? comments](comments: show / hide)

    ghci> 

[?? comments](comment: add)

E se queremos uma função que se comporta como `tails`, mas que retorna _apenas_ os sufixos não vazios? Uma possibilidade seria para nós a escrever a nossa própria versão a mão. Vamos usar uma nova peça de notação, o símbolo `@`.[?? comments](comments: show / hide)

    -- arquivo: ca04/ArvorDeSufixos.hs

[?? comments](comments: show / hide)

O padrão `xs@(_:xs')` é chamado um _padrão as_, e significa “ligar o variável `xs` para o valor que corresponda ao lado direito do símbolo `@`”.[?? comments](comments: show / hide)

No nosso exemplo, se o padrão depois do “@” corresponde, `xs` será obrigado a toda a lista que combinava e `xs'` para todos, mas o cabeça da lista (usamos o padrão wild card `_` para indicar que estamos não está interessado no valor do cabeça de lista).[?? comments](comments: show / hide)

    ghci> 

[?? comments](comment: add)

O padrão as torna o código nosso mais legível. Para ver como isso ajuda, vamos comparar uma definição que não tenha um padrão as.[?? comments](comments: show / hide)

    -- arquivo: ca04/ArvorDeSufixos.hs

[?? comments](comments: show / hide)

Aqui, a lista que nós desconstruído no padrão de jogo só fica colocada de volta em conjunto no corpo da função.[?? comments](comments: show / hide)

Padrões as ter um uso mais prático do que a leitura simples: eles podem nos ajudar a compartilhar dados, em vez de copiá-lo. Em nossa definição de `semPadrãoAs`, quando jogo `(x:xs)`, vamos construir uma nova cópia dele no corpo da nossa função. Isso nos leva a atribuir um nó nova lista em tempo de execução. Isso pode ser barato, mas não é livre. Em contraste, quando nós definimos `sufixos`, reutilizadas o valor `xs` que nós combinamos com o nosso como padrão. Desde que reutilizar um valor existente, evitamos uma atribuição pouco.[?? comments](comments: show / hide)

Reutilização de código através da composição
--------------------------------------------

Parece uma vergonha para introduzir uma nova função, `sufixos`, que faz quase a mesma coisa que a função existente `tails`. Certamente nós podemos fazer melhor?[?? comments](comments: show / hide)

Lembre-se da função `init` introduzimos na [seção chamada “Trabalhar com as listas”](#fp.lists "seção chamada “Trabalhar com as listas”"): retorna todos, mas o último elemento de uma lista.[?? comments](comments: show / hide)

    -- arquivo: ca04/ArvorDeSufixos.hs

[?? comments](comments: show / hide)

Esta função `sufixos2` funciona igualmente a `sufixos`, mas é um única linha de código.[?? comments](comments: show / hide)

    ghci> 

[?? comments](comment: add)

Se tomarmos um passo para trás, vemos o reflexo de um padrão aqui: nós estamos aplicando uma função, em seguida, aplicar uma outra função para o seu resultado. Vamos transformar esse padrão em uma definição de função.[?? comments](comments: show / hide)

    -- arquivo: ca04/ArvorDeSufixos.hs

[?? comments](comments: show / hide)

Agora temos uma função, `compor`, que podemos usar para “cola” outras duas funções em conjunto.[?? comments](comments: show / hide)

    -- arquivo: ca04/ArvorDeSufixos.hs
    sufixos3 xs = compor init tails xs

[?? comments](comments: show / hide)

O currying automático do Haskell nos deixa cair a variável `xs`para que possamos fazer a nossa definição ainda mais curtos.[?? comments](comments: show / hide)

    -- arquivo: ca04/ArvorDeSufixos.hs
    sufixos4 = compor init tails

[?? comments](comments: show / hide)

Felizmente, não precisamos de escrever a nossa própria função `compor`. Ligar funções em cada um, como isto é tão comum que a Prelude fornece composição das funções através do operador `(.)`.[?? comments](comments: show / hide)

    -- arquivo: ca04/ArvorDeSufixos.hs
    sufixos5 = init . tails

[?? comments](comments: show / hide)

O operador `(.)` não é uma parte especial da sintaxe da linguagem, é apenas um operador normal.[?? comments](comments: show / hide)

    ghci> 

[?? comments](comment: add)

Podemos criar novas funções a qualquer momento por escrito cadeias de funções compostas, costurado com `(.)`, tanto tempo (é claro) como o tipo de resultado da função no lado direito de cada um `(.)` corresponde ao tipo de parâmetro que o função na esquerda pode aceitar.[?? comments](comments: show / hide)

Como exemplo, vamos resolver um enigma muito simples: a contagem do número de palavras em uma seqüência que começa com uma letra maiúscula.[?? comments](comments: show / hide)

    ghci> 

[?? comments](comment: add)

Podemos entender que esta função é composta pela análise das suas peças. A função `(.)` é associativa direito, por isso vamos prosseguir da direita para a esquerda.[?? comments](comments: show / hide)

    ghci> 

[?? comments](comment: add)

A função `words` tem um tipo de resultado de \[String\], para o que está no lado esquerdo de `(.)` deve aceitar um argumento compatível.[?? comments](comments: show / hide)

    ghci> 

[?? comments](comment: add)

Essa função retorna `True` se uma palavra começa com uma letra maiúscula (testá-lo em **ghci**), os `filter (isUpper . head)` retorna uma lista de Strings contendo apenas palavras que começam com letras maiúsculas.[?? comments](comments: show / hide)

    ghci> 

[?? comments](comment: add)

Uma vez que esta expressão retorna uma lista, tudo o que resta é calcular o comprimento da lista, o que fazemos com outra composição.[?? comments](comments: show / hide)

Aqui está outro exemplo, retirado de uma aplicação real. Queremos extrair uma lista de nomes de macro de um arquivo de cabeçalho C acompanha `libpcap`, uma biblioteca popular pacote de filtragem de rede. O arquivo de cabeçalho contém um grande número de definições da seguinte forma.[?? comments](comments: show / hide)

#define DLT_EN10MB      1       /* Ethernet (10Mb) */
#define DLT_EN3MB       2       /* Experimental Ethernet (3Mb) */
#define DLT_AX25        3       /* Amateur Radio AX.25 */

[?? comments](comments: show / hide)

Nosso objetivo é extrair nomes como `DLT_EN10MB` e `DLT_AX25`.[?? comments](comments: show / hide)

    -- arquivo: ca04/dlts.hs

[?? comments](comments: show / hide)

Nós tratamos todo um arquivo como uma String, dividi-lo com `lines`, em seguida, aplicar `foldr passo []` para a lista resultante de linhas. A função de auxiliar `passo` opera em uma única linha.[?? comments](comments: show / hide)

    -- arquivo: ca04/dlts.hs

[?? comments](comments: show / hide)

Se coincidir com uma definição de macro com a nossa expressão guarda, podemos contras o nome da macro para a cabeça da lista que está retornando, caso contrário, deixamos a lista intocada.[?? comments](comments: show / hide)

Enquanto as funções individuais do corpo de `palavra2` estão agora familiar para nós, pode levar um pouco de prática para montar uma cadeia de composições como esta. Vamos examinar o processo.[?? comments](comments: show / hide)

Mais uma vez, procede da direita para a esquerda. A primeira função é `words`. [?? comments](comments: show / hide)

    ghci> 

[?? comments](comment: add)

Em seguida, aplicamos `tail` para o resultado de `words`.[?? comments](comments: show / hide)

    ghci> 

[?? comments](comment: add)

Finalmente, aplicando `head` para o resultado de `drop 1 . words` nos dará o nome de nossa macro.[?? comments](comments: show / hide)

    ghci> 

[?? comments](comment: add)

### Use a cabeça sabiamente

Depois da advertência contra lista de funções inseguras na [seção chamada “Trabalhar segura e saudavelmente a com funções crashy”](#fp.lists.safe "seção chamada “Trabalhar segura e saudavelmente a com funções crashy”"), aqui estamos chamando tanto a `head` e a `tail`, duas dessas funções de lista inseguro. O Que Dá?[?? comments](comments: show / hide)

Neste caso, podemos nos assegurar de inspeção que estamos seguros de uma falha de execução. O guarda padrão na definição de `passo` contém duas palavras, por isso quando nós aplicamos `words` a qualquer String de palavras que faz passar pelo guarda, que vamos ter uma lista de pelo menos dois elementos, `"#define"` e alguns macro iniciando com `"DLT_"`.[?? comments](comments: show / hide)

Este tipo de raciocínio que devemos fazer para nos convencermos de que nosso código não vai explodir quando chamamos funções parciais. Não se esqueça nossa admoestação anterior: chamar funções inseguro como este requer cuidados, e muitas vezes pode tornar o código mais frágil de maneira sutil. Se por algum motivo, modificou o padrão de proteção para conter apenas uma palavra, poderíamos nos expor à possibilidade de um acidente, como o corpo da função assume que receberá duas palavras.[?? comments](comments: show / hide)

Dicas para escrever código legível
----------------------------------

Até agora, neste capítulo, me deparei com duas características tentador olhar de Haskell: recursão de cauda e funções anônimas. Tão agradável como estes são, muitas vezes não se deseja usá-los.[?? comments](comments: show / hide)

Muitas operações de manipulação de lista pode ser mais facilmente expressos usando combinações de funções de biblioteca, tais como `map`, `take`, e `filter`. Sem dúvida, isto requer alguma prática para se acostumar com estes. No retorno para nosso investimento inicial, podemos ler e escrever código mais rapidamente e com menos bugs.[?? comments](comments: show / hide)

A razão para isso é simples. Uma definição de função recursiva de cauda tem o mesmo problema como um loop em uma linguagem imperativa: é completamente geral. Ele pode realizar alguns filtragem, mapeamento de alguns, ou quem sabe mais o quê. Somos obrigados a olhar em detalhe toda a definição da função para ver o que ele está realmente fazendo. Em contraste, `map` e funções de manipulação mais outra lista fazer apenas _uma_ coisa. Podemos tomar como garantido que estes blocos de construção simples fazer, e focar na idéia de que o código é tentar expressar, não os mínimos detalhes de como é a manipulação de seus insumos.[?? comments](comments: show / hide)

No meio do caminho entre a cauda funções recursivas (com a generalidade completo) e nossa caixa de ferramentas de funções de manipulação de lista (cada um deles faz uma coisa) encontram-se as dobras. Uma dobra exige mais esforço para entender que, digamos, uma composição de `map` e `filter`que faz a mesma coisa, mas ele se comporta de forma mais regular e previsível do que uma função recursiva de cauda. Como regra geral, não use uma dobra se você pode compor algumas funções da biblioteca, mas caso contrário tenta usar uma dobra de preferência à mão-rolados uma loop uma recursiva cauda.[?? comments](comments: show / hide)

Para as funções anônimas, eles tendem a interromper o “fluxo” de ler um pedaço de código. É muitas vezes tão fácil de escrever uma definição de função local em um cláusula `let` ou `where`, e usar isso, como é para colocar uma função anônima em seu lugar. As vantagens relativas de uma função chamada são dois: não precisamos entender a definição da função quando estamos lendo o código que usa-lo, e um nome de função bem escolhido age como um pequeno pedaço de documentação local.[?? comments](comments: show / hide)

Space leaks e avaliação rigorosa
--------------------------------

A função `foldl` que discutimos anteriormente não é o único lugar onde podem ocorrer vazamentos espaço no código Haskell. Vamos usá-lo para ilustrar como a avaliação não-estrita às vezes pode ser problemático, e como resolver as dificuldades que podem surgir.[?? comments](comments: show / hide)

![[Tip]](support/figs/tip.png)

Você precisa saber de tudo isso agora?

É perfeitamente razoável para pular esta seção até que você encontrar um space leak “in the wild”. Desde que você usa `foldr` se você estiver gerando uma lista, e `foldl'` em vez de `foldl` contrário, vazamentos de espaço não são susceptíveis de incomodá-lo na prática por um tempo.[?? comments](comments: show / hide)

### Evitar space leaks com seq

Nós nos referimos a uma expressão que não é avaliada preguiçosamente tão _rigorosa_, tão `foldl'` é uma rigorosa deixou desistir. Ele ignora avaliação usual Haskell não-estrita através da utilização de uma função chamada `seq`.[?? comments](comments: show / hide)

    -- arquivo: ca04/Fold.hs

[?? comments](comments: show / hide)

Esta função `seq` tem um tipo peculiar, insinuando que ele não está jogando com as regras habituais.[?? comments](comments: show / hide)

    ghci> 

[?? comments](comment: add)

Ele funciona da seguinte forma: quando uma expressão `seq` é avaliada seguintes, ele força o seu primeiro argumento a ser avaliada, em seguida, retorna seu segundo argumento. Na verdade, não fazer nada com o primeiro argumento: `seq` existe apenas como uma maneira de forçar que o valor a ser avaliada. Vamos caminhar através de uma aplicação breve para ver o que acontece.[?? comments](comments: show / hide)

    -- arquivo: ca04/Fold.hs

[?? comments](comments: show / hide)

Isso expande o seguinte.[?? comments](comments: show / hide)

    -- arquivo: ca04/Fold.hs

[?? comments](comments: show / hide)

O uso de `seq` avalia forçada `novo` a `3`, e retorna seu segundo argumento.[?? comments](comments: show / hide)

    -- arquivo: ca04/Fold.hs

[?? comments](comments: show / hide)

Acabamos com o resultado seguinte.[?? comments](comments: show / hide)

    -- arquivo: ca04/Fold.hs

[?? comments](comments: show / hide)

Graças a `seq`, não há thunks à vista.[?? comments](comments: show / hide)

### Aprender a usar o seq

Sem algum sentido, existe um elemento de mistério para usar efetivamente seguintes. Aqui estão algumas regras úteis para usá-lo bem.[?? comments](comments: show / hide)

Para ter algum efeito, uma expressão `seq` devem ser a primeira coisa avaliada em uma expressão.[?? comments](comments: show / hide)

    -- arquivo: ca04/Fold.hs
    -- incorreta: seq é escondida pela aplicação de alguma_funcao desde 
    -- algumaFuncao será avaliada primeiro, seq pode ocorrer muito tarde

[?? comments](comments: show / hide)

Para estritamente avaliar vários valores, aplicações da cadeia de `seq` juntos.[?? comments](comments: show / hide)

    -- arquivo: ca04/Fold.hs

[?? comments](comments: show / hide)

Um erro comum é tentar utilizar `seq` com duas expressões independentes.[?? comments](comments: show / hide)

    -- arquivo: ca04/Fold.hs

[?? comments](comments: show / hide)

Aqui, a intenção aparente é o de avaliar estrita `step zero x`. Uma vez que a expressão é repetido no corpo da função, estritamente avaliar a primeira instância de que não terá nenhum efeito sobre o segundo. A utilização de `let` partir da definição de acima `foldl'` mostra como conseguir este efeito corretamente.[?? comments](comments: show / hide)

Ao avaliar uma expressão, `seq` pára logo que se chega a um construtor. Para os tipos simples, como números, isso significa que irá avaliá-los completamente. tipos de dados algébricos são uma história diferente. Considere o valor `(1+2):(3+4):[]`. Se aplicarmos `seq` para isso, vai avaliar o thunk `(1+2)`. Uma vez que ele irá parar quando atingir o primeiro construtor `(:)`, ele não terá nenhum efeito sobre a conversão segundo. O mesmo é verdadeiro para tuplas: `seq ((1+2),(3+4)) True` não fará nada para o thunks dentro do par, uma vez que imediatamente bate construtor do par.[?? comments](comments: show / hide)

Se necessário, podemos utilizar técnicas habituais de programação funcional para contornar essas limitações.[?? comments](comments: show / hide)

    -- arquivo: ca04/Fold.hs

[?? comments](comments: show / hide)

É importante compreender que a `seq` não é livre: ele tem que executar uma verificação em tempo de execução para ver se uma expressão foi avaliada. Use com moderação. Por exemplo, enquanto a nossa função `parEstrito` avalia o conteúdo de um par até o primeiro construtor, ele adiciona as despesas gerais da correspondência padrão, duas aplicações de `seq`, e da construção de uma nova tupla. Se fôssemos medir o seu desempenho no circuito interno de um referência, podemos encontrá-lo para tornar o programa lento.[?? comments](comments: show / hide)

Além do seu custo de desempenho se em demasia, `seq` não é um milagre cura para todos os problemas de consumo de memória. Só porque você _pode_ avaliar algo estritamente não significa que você _deve_. O uso descuidado do `seq` podem fazer nada; vazamentos mover espaço existente ao redor, ou introduzir novos vazamentos.[?? comments](comments: show / hide)

Os melhores guias para se `seq` é necessário, e como ele funciona, são medidas de desempenho e perfil, a qual será abordada no [Chapter 25, _Profiling e ajuste de desempenho_](profiling-and-optimization.html "Chapter 25, Profiling e ajuste de desempenho"). A partir de uma base de medição empírica, você irá desenvolver um senso de confiança de quando `seq` é mais útil.[?? comments](comments: show / hide)

  

* * *

\[[8](#id591518)\] Infelizmente, não temos espaço para abordar esse desafio neste livro.

\[[9](#id594951)\] A barra invertida foi escolhido por sua semelhança visual com a letra grega lambda, `λ`. Embora GHC pode aceitar a entrada Unicode, ele trata corretamente `λ` como uma letra, não como sinônimo de `\`.

![](support/figs/rss.png) Quer ficar atualizado? Assine o feed comentário para [este capítulo](http://book.realworldhaskell.org/feeds/comments/fp/), ou o [livro inteiro](http://book.realworldhaskell.org/feeds/comments/).

Copyright 2007, 2008 Bryan O'Sullivan, Don Stewart e John Goerzen. Esta obra está licenciada sob uma [Creative Commons Attribution-Noncommercial 3.0 License](http://creativecommons.org/licenses/by-nc/3.0/). Ícones por [Paul Davey](mailto:mattahan@gmail.com) aka [Mattahan](http://mattahan.deviantart.com/). Tradução por Doralice de Araujo Chaves, Sergio Souza Costa, Nick Rudnick e [Google Translate](http://www.google.com/).

[Anterior](types-and-functions.html) 

 

 [Próximo](writing-a-library-working-with-json-data.html)

Capítulo 3. Definir os tipos, racionalizando as funções 

[Casa](index.html)

 Capítulo 5. Escrever uma biblioteca: trabalhar com dados JSON

_uacct = 'UA-1805907-3'; urchinTracker();
