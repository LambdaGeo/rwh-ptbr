
Capítulo 21. Usando Bancos de Dados
-----------------------------------

Tudo desde fóruns a podcatchers ou até mesmo programas de backup freqüentemente usam bancos de dados para armazenamento persistente. Bancos de dados baseados em SQL são bastante convenientes: eles são rápidos, a escala de tamanhos pode variar de minúsculo a enorme, pode operar através da rede, frequentemente ajuda a lidar com travamentos e operações, e pode ainda fornecer tratamento de erros e melhorias em redundância para aplicações. Bases de dados vêm em diferentes formas: a grandes bases de dados comerciais, tais como Oracle, engines de código aberto, como MySQL ou PostgreSQL, e ainda engines integradas como Sqlite. [6 comments](comments: show / hide)

Motivos pelos quais os bancos de dados são tão importantes, mostra como o suporte Haskell para eles também é importante. Neste capítulo, vamos apresentar um dos frameworks Haskell para trabalhar com bancos de dados. Também vamos usar esse framework para começar a construir um podcast downloader, que será desenvolvido no Capítulo 22, Exemplo Extendido: Web Client Programming.[Chapter 22, _Extended Example: Web Client Programming_](http://book.realworldhaskell.org/read/extended-example-web-client-programming.html "Chapter 22. Extended Example: Web Client Programming"). [2 comments](comments: show / hide)

Resumo do HDBC
--------------

Na parte inferior da pilha do banco de dados está a engine do banco de dados. A engine do banco de dados é responsável por armazenar os dados no disco. Engines de Bancos de dados conhecidas incluem PostgreSQL, MySQL e Oracle. [4 comments](comments: show / hide)

A maioria das engines de bancos de dados modernas suportam SQL, Structured Query Language, como uma forma padrão de obter dados de entrada e saída de bancos de dados relacionais. Este livro não irá fornecer um tutorial sobre SQL ou gerenciamento de banco de dados relacional. \[[49](http://book.realworldhaskell.org/read/using-databases.html#ftn.id667806)\] [3 comments](comments: show / hide)

Uma vez que você utiliza uma engine de banco de dados que suporta SQL, você precisa encontrar uma maneira de se comunicar com ela. Cada banco de dados tem seu próprio protocolo. Desde que SQL é razoavelmente constante em bases de dados, é possível fazer uma interface genérica que usa os drivers para cada protocolo individualmente. [No comments](comment: add)

Haskell tem vários frameworks de bancos de dados diferentes disponíveis, fornecendo algumas camadas de alto nível que sobrepõe umas as outras. Para este capítulo, vamos nos concentrar no HDBC, o sistema Haskell Data Base Connectivity. HDBC é uma biblioteca de abstração de banco de dados. Ou seja, você pode escrever código que usa HDBC e pode acessar dados armazenados em quase qualquer banco de dados SQL com pouca ou nenhuma modificação. \[[50](http://book.realworldhaskell.org/read/using-databases.html#ftn.id667844)\] Mesmo que você nunca precise mudar as engines de banco de dados subjacentes, o sistema HDBC de drivers faz com que um grande número de opções disponíveis para você com uma única interface. [4 comments](comments: show / hide)

Outra biblioteca de abstração para o banco de dados para Haskell é a HSQL, que compartilha um propósito semelhante ao HDBC. Há também um framework de alto nível chamado HaskellDB, que fica sobre um HDBC ou HSQL, e é projetado para ajudar a isolar o programador dos detalhes de trabalhar com SQL. No entanto, ele não tem como apelo mais amplo porque o projeto limita a certos - embora bastante comum - padrões de acesso ao banco. Finalmente, Takusen é um framework que utiliza uma abordagem "left fold" para a leitura de dados do banco de dados. [5 comments](comments: show / hide)

Instalando HDBC e Drivers
-------------------------

Para conectar um dado banco de dados com o HDBC, você precisa de ao menos dois pacotes: a interface genérica, e um driver específico para o seu banco de dados. Você pode obter o pacote genérico HDBC, e todos os outros drivers do [Hackage](http://hackage.haskell.org/)\[[51](http://book.realworldhaskell.org/read/using-databases.html#ftn.id667890)\]. Para este capítulo, utilizaremos o HDBC versão 1.1.3 para exemplos. [6 comments](comments: show / hide)

Você também vai precisar de um banco de dados de backend e um driver backend. Para este capítulo, vamos usar a versão SQLite 3. SQLite é um banco de dados integrado, por isso não requer um servidor separado e é fácil de configurar. Muitos sistemas operacionais já vêm com o SQLite versão 3. Se o seu não, você pode baixá-lo em[http://www.sqlite.org/](http://www.sqlite.org/). A homepage do HDBC tem um link para conhecidos HDBC backend drivers. O driver específico para o SQLite versão 3 pode ser obtido no Hackage. [2 comments](comments: show / hide)

Se você quiser usar HDBC com outras bases de dados, confira os drivers HDBC conhecidos na página [http://software.complete.org/hdbc/wiki/KnownDrivers](http://software.complete.org/hdbc/wiki/KnownDrivers). Lá você encontrará um link para a ligação ODBC, que permite você se conectar a praticamente qualquer banco de dados em praticamente qualquer plataforma (Windows, POSIX, e outros). Você encontrará também uma ligação PostgreSQL. MySQL é suportado através da ligação ODBC, e informações específicas para usuários do MySQL podem ser encontradas na [HDBC-ODBC API documentation](http://software.complete.org/static/hdbc-odbc/doc/HDBC-odbc/). [9 comments](comments: show / hide)

Conectando ao Banco de Dados
----------------------------

Para conectar a um banco de dados, você vai usar uma função de conexão de um driver de backend de banco de dados. Cada banco tem seu próprio método único de conexão. A conexão inicial é geralmente a única vez que você vai chamar qualquer coisa de um módulo de driver backend diretamente. [No comments](comment: add)

A função de conexão com o banco de dados irá devolver um identificador banco de dados. O tipo exato deste identificador pode variar de um driver para outro, mas será sempre uma instância da `IConnection` typeclass.Todas as funções que você irá usar para operar em bancos de dados irá trabalhar com qualquer tipo que seja uma instância de`IConnection`. Quando você terminar de falar com o banco de dados, chame a função `disconnect`. Ele irá desconectá-lo do banco de dados. Aqui está um exemplo de conexão com um banco de dados SQLite: [No comments](comment: add)

    ghci> 

[5 comments](comments: show / hide)

Transações
----------

A maioria dos SGBDs modernos possuem controle de regras para as transações. Este controle visa garantir que todos os componentes de uma modificação sejam aplicados, ou que nenhum deles seja. Além disso, este controle de transações impede que outros processos acessando o mesmo banco de dados de acessar dados de alterações parciais que estão em andamento. [2 comments](comments: show / hide)

Muitos SGBDs exigem explicitamente que seja realizado um commit(comando da DTL – Linguagem de Transação de Dados - que envia todos os dados das mudanças permanentemente) antes que as mudanças sejam gravadas no disco. No modo "autocommit" um commit implícito é executado após cada declaração. Isso pode fazer o ajuste de bancos de dados mais facilmente para os programadores não acostumados com o SGBD, mas é apenas um obstáculo para as pessoas que realmente querem usar transações multi-instrução. [4 comments](comments: show / hide)

HDBC intencionalmente não suporta o modo AUTOCOMMIT. Quando você modificar dados em seus bancos de dados você deve explicitamente fazer um COMMIT para que as alterações sobre os dados sejam gravadas em disco. Existem duas maneiras de fazer isso no HDBC: pode-se chamar `commit` quando você estiver pronto para escrever dados no disco, ou você pode usar a função `withTransaction` que trabalha com os dados em torno das modificações. `withTransaction` fará com que os dados alterados sofram COMMIT após a conclusão bem-sucedida da transação. [1 comment](comments: show / hide)

Às vezes, um problema ocorre enquanto se está trabalhando na gravação de dados no disco. Talvez, perceba-se um erro do SGBD ou descubra-se um problema com os dados. Nesses casos, você pode "reverter" as alterações. Isso fará com que todas as modificações que foram feitas no ultimo `commit` sejam esquecidas. Em HDBC, você pode chamar o comando `rollback` para reverter o ultimo `commit`. Se você estiver usando `withTransaction`, qualquer exceção não capturada fará um `rollback` para ser efetivada. [No comments](comment: add)

Note que o `rollback` reverte as operações da última `commit`, `rollback`, ou `withTransaction`. Um banco de dados não mantém um extenso histórico das operações, como um controle de versão do sistema. Você verá exemplos de `commit` mais tarde neste capítulo. [6 comments](comments: show / hide)

![[Warning]](./Chapter 21. Using Databases_files/warning.png)

Aviso

Um banco de dados popular, o MySQL, não suporta transações com seu tipo de tabela padrão. Na sua configuração padrão, o MySQL irá ignorar chamadas para `commit` ou `rollback` e irá gravar todas as alterações no disco imediatamente. O driver ODBC HDBC tem instruções para configurar o MySQL para indicar ao HDBC que ele não suporta estes comandos de transações DTL, o que fará o `commit` e `rollback` gerar erros. Alternativamente, você pode usar tabelas InnoDB no MySQL, que realiza operações de apoio. Tabelas InnoDB são recomendadas para uso com HDBC. [No comments](comment: add)

Pequisas Simples
----------------

Algumas das mais simples consultas em SQL envolvem declarações que não retornam todos os dados. Essas consultas podem ser usadas para criar tabelas, inserir dados, apagar dados e definir os parâmetros do banco de dados. [No comments](comment: add)

A função básica para o envio de consultas ao banco de dados é `run`. Esta função tem um `IConnection`, uma `String` que representa a consulta em si, e uma lista de parâmetros. Vamos usá-lo para configurar algumas coisas na nossa base de dados. [No comments](comment: add)

    ghci> 

[6 comments](comments: show / hide)

Depois de se conectar ao banco de dados, criamos previamente uma tabela chamada `test`. Então, inserimos uma linha de dados na tabela. Finalmente, demos um `commit`, gravamos a mudança no disco e desconectamos do banco de dados. Note que se não tivéssemos chamado `commit`, nenhuma mudança final teria sido gravada no banco de dados. [No comments](comment: add)

A função `run` retorna o número de linhas modificadas pela consulta. Para a primeira consulta, que criou uma tabela, nenhuma linha foi modificada. A segunda consulta de inserção de dados na tabela que inseriu uma única linha a função `run` retornou `1`.. [No comments](comment: add)

SqlValues
---------

Antes de prosseguir, precisamos discutir um tipo de dados introduzidos no HDBC: `SqlValue`. Uma vez que ambos, Haskell e SQL são fortemente tipadas o HDBC tenta preservar informações sobre o tipo. Ao mesmo tempo, os tipos do Haskell e do SQL não exatamente idênticos. Além disso, os bancos de dados diferentes têm maneiras diferentes de representar os seus tipos como datas ou caracteres especiais em strings. [5 comments](comments: show / hide)

`SqlValue` é um tipo de dados que tem um número de construtores igual ao do `SqlString`, `SqlBool`, `SqlNull`, `SqlInteger`, etc. Isso permite que você represente vários tipos de dados no banco de dados, e acessar vários tipos de dados nos resultados das consultas, e ainda armazená-los, para isto existem as funções `toSql` e `fromSql` que são normalmente usadas para este fim. Se você se preocupa com a representação exata dos dados você ainda pode construir manualmente as `SqlValue` dos dados se você precisar. [1 comment](comments: show / hide)

Parâmetros de Busca
-------------------

HDBC, como a maioria dos bancos de dados, suporta um conceito de parâmetros substituíveis em consultas. Há três benefícios primários de usar parâmetros substituíveis: eles previnem erros nos SQL ou dificuldade quando a entrada contiver caracter especial, eles melhoram desempenho ao executar consultas semelhantes repetidamente, e eles permitem inserção fácil e portátil de dados em consultas. [8 comments](comments: show / hide)

Digamos que você quer adicionar milhares de filas em nossa nova tabela `test`. Você poderia emitir milhares de consultas que se parecem `INSERT INTO test VALUES (0,'zero')` e `INSERT INTO test VALUES (1,'one')`. Isto força o servidor de banco de dados a analisar gramaticalmente cada declaração de SQL individualmente. Se você pudesse substituir os dois valores por um endereço de memória, o servidor poderia analisar gramaticalmente o SQL com uma consulta, executando em tempos múltiplos com os dados diferentes. [No comments](comment: add)

Um segundo problema envolve caracter de salvamento. Se você quisesse inserir a string `"I don't like 1"`? SQL usa o único caracter especial para mostrar o fim do campo. A maioria dos bancos de dados de SQL lhe exigiria que escrevesse dessa forma `'I don''t like 1'`. Mas regras para outros caracteres especiais, como backslashes, diferem entre bancos de dados. Em lugar de tentar codificar isto você mesmo, o HDBC pode controlar tudo para você. Veja um exemplo. [4 comments](comments: show / hide)

    ghci> 

[3 comments](comments: show / hide)

Os pontos de interrogação na pesquisa INSERT neste exemplo são os endereços de memória. Nós passamos os parâmetros que serão encaminahdos para lá. `run` leva uma lista de `SqlValue`, então nós usamos `toSql` para converter cada item em um `SqlValue`. O HDBC automaticamente controla a conversão no uso da `String` `"zero"` na representação apropriada para o banco de dados. [No comments](comment: add)

Esta aproximação na verdade não alcançará qualquer desempenho benéfico ao inserir grandes quantidades de dados. Para isso, nós precisamos de mais controle em cima do processo de criar a consulta SQL. [No comments](comment: add)

![[Note]](./Chapter 21. Using Databases_files/note.png)

Usando Declarações Realocáveis

Declarações Realocáveis apenas trabalham como parte de consultas onde o servidor é esperado como valor, como uma cláusula onde uma declaração SELECT ou um valor para uma declaração INSERT. Você não pode declarar `run "SELECT * from ?" [toSql "tablename"]` e epserar que funcione. Uma nome tabela não é um valor, e a maioria dos bancos de dados não aceitará esta sintaxe. Na prática não é um grande problema, porque raramente chamadas para recolocar coisas que não são valores são chamadas deste modo. [4 comments](comments: show / hide)

Declarações Preparadas
----------------------

HDBC define uma função `prepare` que preparará uma consulta de SQL, mas ainda não liga os parâmetros à consulta. `prepare` retorna `Statement` que representa a consulta compilada. [3 comments](comments: show / hide)

Uma vez você tem uma `Statement`, você pode fazer várias coisas com isto. Você pode chamar `execute` nisto um ou mais vezes. Depois de chamar `execute` em uma consulta que retorna dados, você pode usar uma função de recuperação para recuperar aqueles dados. Funções como `run` e `quickQuery'` usam declarações e a função `execute` interiormente; eles simplesmente são atalhos para deixar você executar tarefas comuns depressa. Quando você precisar de mais controle sobre o que está acontecendo, você pode usar uma `Statement` em vez de uma função como `run`. [2 comments](comments: show / hide)

Veja que usar declarações para inserir valores múltiplos com uma única consulta. Aqui está um exemplo: [No comments](comment: add)

    ghci> 

[No comments](comment: add)

Neste exemplo, nós criamos uma declaração preparada e chamamos de `stmt`. Nós executamos aquela declaração então quatro vezes, e passamos parâmetros diferentes a cada hora. Estes parâmetros são usados, em ordem, para substituir os pontos de interrogação na String de consulta original. Finalmente, fazemos as mudanças e desconectamos o banco de dados. [1 comment](comments: show / hide)

HDBC também provê uma função `executeMany` que pode ser útil em situações como esta. A função `executeMany` leva uma lista de filas de dados simplesmente para chamar a declaração. Aqui está um exemplo: [2 comments](comments: show / hide)

    ghci> 

[No comments](comment: add)

![[Note]](./Chapter 21. Using Databases_files/note.png)

Execução mais eficiente

No servidor, a maioria dos bancos de dados terão uma otimização que eles podem aplicar `executeMany` para então ter de compilar esta string consulta uma vez, ao invés de duas. \[[52](http://book.realworldhaskell.org/read/using-databases.html#ftn.id668986)\] Isto pode levar a um ganho dramático de memória quando a inserção usa grandes volumes de dados de uma vez. Alguns bancos de dados podem também aplicar esta otimização ao `execute`, mas não a tudo. [5 comments](comments: show / hide)

Leitura dos Resultados
----------------------

Até agora, discutimos as consultas que inserem ou alteram dados. Vamos discutir a obtenção de dados de volta para fora do banco de dados. O tipo da função `quickQuery'` se assemelha muito a `run`, mas ele retorna uma lista de resultados, em vez de uma contagem de linhas alteradas. `quickQuery'` é normalmente é usado com SELECT. Vamos ver um exemplo: [2 comments](comments: show / hide)

    ghci> 

[5 comments](comments: show / hide)

`quickQuery'` trabalha com parâmetros substituíveis, como discutimos acima. Neste caso, não estamos usando nenhum, então o conjunto de valores para substituir é uma lista vazia no final da chamada do `quickQuery'`. `quickQuery'` retorna uma lista de linhas, onde cada linha é em si representado como `[SqlValue]`. Os valores da linha são listados na ordem retornada pelo banco de dados. Pode-se usar `fromSql` para convertê-las em tipos regulares Haskell, conforme necessário. [No comments](comment: add)

É um pouco difícil de ler a saída. Vamos estender este exemplo para formatar bem os resultados. Aqui está um código para fazer isso: [No comments](comment: add)

\-\- file: ch21/query.hs
import Database.HDBC.Sqlite3 (connectSqlite3)
import Database.HDBC

{\- | Definir uma função que recebe um inteiro que representa o máximo id do valor acima.
Buscar todas as linhas correspondentes do banco de dados de teste e 
imprimi-las para a tela em um formato amigável. -}
query :: Int -> IO ()
query maxId = 
    do -- Connect to the database
       conn <- connectSqlite3 "test1.db"

       \-\- Run the query and store the results in r
       r <- quickQuery' conn
            "SELECT id, desc from test where id <= ? ORDER BY id, desc"
            \[toSql maxId\]

       \-\- Convert each row into a String
       let stringRows = map convRow r
                        
       \-\- Print the rows out
       mapM_ putStrLn stringRows

       \-\- And disconnect from the database
       disconnect conn

    where convRow :: \[SqlValue\] -> String
          convRow \[sqlId, sqlDesc\] = 
              show intid ++ ": " ++ desc
              where intid = (fromSql sqlId)::Integer
                    desc = case fromSql sqlDesc of
                             Just x -> x
                             Nothing -> "NULL"
          convRow x = fail $ "Unexpected result: " ++ show x

[4 comments](comments: show / hide)

Este programa é essencialmente a mesma coisa que o nosso exemplo com **ghci**, mas com uma novidade: a função `convRow`. Esta função tem uma linha de dados do banco de dados e converte para uma `String`. Essa string pode então ser facilmente impressa. [2 comments](comments: show / hide)

Observe como tomamos `intid` de `fromSql` diretamente, mas o `fromSql sqlDesc` processado como um tipo `Maybe String`. Se você lembrar, declaramos que a primeira coluna desta tabela não pode conter um valor nulo, mas que a segunda coluna poderia. Portanto, podemos ignorar o potencial para um nulo na primeira coluna, mas não na segunda. É possível usar `fromSql` para converter a segunda coluna para uma `String` diretamente, e seria o mesmo trabalho - até que uma linha com nulo na posição seja encontrado, o que causaria uma exceção do tempo de execução. Então, nós convertemos um valor nulo do SQL na string `"NULL"`. Quando impresso, isso será equivalente a uma string SQL `'NULL'`, mas que é aceitável para este exemplo. Vamos tentar chamar essa função em **ghci**: [1 comment](comments: show / hide)

    ghci> 

[No comments](comment: add)

### Leitura de Declarações

Como discutimos na seção chamada [“Declarações Preparadas”](http://book.realworldhaskell.org/read/using-databases.html#databases.statements "Prepared Statements"), você pode usar as instruções para a leitura. Existem várias maneiras de leitura de dados a partir de instruções que podem ser úteis em determinadas situações. Como a `run`, `quickQuery'` são funções de conveniência que na verdade usam instruções para realizar sua tarefa. [No comments](comment: add)

Para criar uma instrução para a leitura use `prepare` do mesmo modo como faria para uma instrução que seria utilizada para gravar dados. Você também pode usar `execute` para executá-la sobre o servidor de banco de dados. Depois, você pode usar várias funções para ler os dados da `Statement`. A função `fetchAllRows'` retorna `[[SqlValue]]`, assim como `quickQuery'`. Há também uma função chamada `sFetchAllRows'`, que converte dados de cada coluna em um `Maybe String` antes de retorná-lo. Finalmente, há `fetchAllRows'`, que retorna `(String, SqlValue)` pares de cada coluna. A String é o nome da coluna retornado pelo banco de dados, veja a seção chamada [“Metadados de Bancos de Dados”](http://book.realworldhaskell.org/read/using-databases.html#databases.metadata "Database Metadata") para outras formas de obter os nomes de coluna. [2 comments](comments: show / hide)

Você também pode ler dados de uma linha por vez, chamada `fetchRow`, que retorna a `IO (Maybe [SqlValue])`. Ela será `Nothing` se todos os resultados já foram lidos, ou uma linha em contrário. [No comments](comment: add)

### Leitura Preguiçosa

Voltando na seção chamada [“Lazy I/O”](http://book.realworldhaskell.org/read/io.html#io.lazy "Lazy I/O"), nós falamos sobre E/S preguiçosa de arquivos. Também é possível ler dados a partir de bases de dados preguiçosamente. Isto pode ser particularmente útil quando se trata de consultas que retornam uma quantidade excepcionalmente grande de dados. Ao ler dados preguiçosamente, você ainda pode usar as funções convenientes, tais como `fetchAllRows` em vez de ter que manualmente ler cada linha como se apresenta. Se você for cuidadoso no uso dos dados, você pode evitar ter todos os dados do buffer na memória. [2 comments](comments: show / hide)

Leitura Preguiçosa de um banco de dados, no entanto, é mais complexa do que a leitura de um arquivo. Quando você terminar de ler preguiçosamente dados de um arquivo, o arquivo é fechado, o que geralmente é bom. Quando você terminar de ler preguiçosamente dados de um banco, a conexão do banco de dados ainda está em aberto - podendo apresentar outras consultas com ele, por exemplo. Alguns bancos de dados podem ainda oferecer suporte a várias consultas simultâneas, assim o HDBC não pode simplesmente fechar a conexão quando estiver pronto. [1 comment](comments: show / hide)

Ao usar a leitura preguiçosa, é extremamente importante terminar de ler o conjunto total de dados antes de tentar fechar a conexão ou executar uma nova consulta. Nós encorajamos você a utilizar as funções estritas, ou de transformação de linha por linha, sempre que possível para minimizar as interações complexas com a leitura preguiçosa. [3 comments](comments: show / hide)

![[Tip]](./Chapter 21. Using Databases_files/tip.png)

Dica

Se você é novo no HDBC ou no conceito de leitura preguiçosa, mas têm grandes quantidades de dados para ler, repetidas chamadas para `fetchRow` pode ser mais fácil de entender. Leitura Preguiçosa é uma ferramenta poderosa e útil, mas deve ser usada corretamente. [No comments](comment: add)

Para ler preguiçosamente um banco de dados, você pode usar as mesmas funções que você usou antes, sem o apóstrofo. Por exemplo, você pode usar no lugar do `fetchAllRows'`, o `fetchAllRows`. Os tipos de funções preguiçosas são as mesmas que seus primos estritos. Aqui está um exemplo de leitura preguiçosa: [No comments](comment: add)

    ghci> 

[1 comment](comments: show / hide)

Note que você poderia ter usado `fetchAllRowsAL'` aqui também. No entanto, se você tivesse um grande conjunto de dados para ler, teria consumido uma grande quantidade de memória. Ao ler os dados preguiçosamente, podemos imprimir conjunstos de resultados extremamente grandes usando uma quantidade constante de memória. Com a versão lenta, os resultados serão avaliado em blocos, com a versão rigorosa, todos os resultado são lidos na frente, armazenadas na memória RAM, em seguida, impressos. [3 comments](comments: show / hide)

Metadados de Banco de Dados
---------------------------

Às vezes pode ser útil para um programa para saber informações sobre o próprio banco de dados. Por exemplo, um programa pode querer ver que existem tabelas de modo que ele possa criar automaticamente as tabelas em falta ou atualizar o esquema do banco de dados. Em alguns casos, um programa pode precisar alterar o seu comportamento, dependendo do banco de dados back-end em uso. [No comments](comment: add)

Primeiro, existe uma função `getTables` que vai obter uma lista das tabelas definidas em um banco de dados. Você também pode usar a função `describeTable`, que irá fornecer informações sobre as colunas definidas em uma determinada tabela. [No comments](comment: add)

Você pode aprender sobre o servidor de banco de dados em uso chamando `dbServerVer` e `proxiedClientName`, por exemplo. A função `dbTransactionSupport` pode ser usada para determinar se um banco de dados oferece, ou não, suporte a transações. Vejamos um exemplo de alguns desses itens: [1 comment](comments: show / hide)

    ghci> 

[No comments](comment: add)

Você também pode aprender sobre os resultados de uma consulta específica e obter informações de sua declaração. A função `describeResult` retorna `[(String, SqlColDesc)]`, uma lista de pares. O primeiro item que dá o nome da coluna, e o segundo fornece informações sobre a coluna: o tipo, o tamanho, e se ele pode ser NULL. A especificação completa é dada na referência API HDBC. [1 comment](comments: show / hide)

Note que algumas bases de dados podem não ser capazes de fornecer todos os metadados. Nestas circunstâncias, uma exceção será levantada. Sqlite3, por exemplo, não suporta `describeResult` ou `describeTable` do modo presente escrito. [1 comment](comments: show / hide)

Tratamento de Erros
-------------------

HDBC utilizará exceções quando erros acontecerem. As exceções têm tipo `SqlError`. Elas carregam informação da máquina de SQL subjacente, como o estado do banco de dados, a mensagem de erro, e o código de erro numérico do banco de dados, etc. [1 comment](comments: show / hide)

**ghc** não sabe exibir um `SqlError` na tela quando acontece. Enquanto a exceção fará o programa terminar, não será exibida uma mensagem útil. Aqui está um exemplo: [1 comment](comments: show / hide)

    ghci> 

[1 comment](comments: show / hide)

Aqui nós tentamos SELECIONAR dados de uma tabela que não existia. A mensagem de erro que voltamos não foi útil. Há uma função utilitária, `handleSqlError` que captura um `SqlError` e aumenta isto como um `IOError`. Desta forma, será exibido um ponto de impressão na tela, mas será mais difícil de extrair pedaços específicos de informação. Vejamos o seu uso: [No comments](comment: add)

    ghci> 

[2 comments](comments: show / hide)

Aqui nós adquirimos mais informações, enquanto incluímos uma declaração de mensagem que não há nenhuma tal tabela como test2. Isto é muito mais útil. Muitos programadores de HDBC fazem disto uma prática padrão para iniciar os programas deles com main = `main = handleSqlError $ do`, que assegurará que todo `SqlError` capturado será imprimido de uma maneira útil. [No comments](comment: add)

Também há `catchSql` e `handleSql` \-\- semelhantes as funções padrão `catch` e `handle` e funções de controle. `catchSql` e `handleSql` interceptarão só erros de HDBC. Para mais informações no tratamento de erros, acesse [Capítulo 19, _Tratamento de Erro_](http://book.realworldhaskell.org/read/error-handling.html "Chapter 19. Error handling"). [No comments](comment: add)

  

* * *

\[[49](http://book.realworldhaskell.org/read/using-databases.html#id667806)\] The O'Reilly books _Learning SQL_ and _SQL in a Nutshell_ may be useful if you don't have experience wiht SQL.

\[[50](http://book.realworldhaskell.org/read/using-databases.html#id667844)\] This assumes you restrict yourself to using standard SQL.

\[[51](http://book.realworldhaskell.org/read/using-databases.html#id667890)\] Para mais informação na instalação de pacotes Haskell, acesse [the section called “Installing Haskell software”](http://book.realworldhaskell.org/read/installing-ghc-and-haskell-libraries.html#installing.haskell.software "Installing Haskell software").

\[[52](http://book.realworldhaskell.org/read/using-databases.html#id668986)\] HDBC

![](./Chapter 21. Using Databases_files/rss.png) Want to stay up to date? Subscribe to the comment feed for [this chapter](http://book.realworldhaskell.org/feeds/comments/databases/), or the [entire book](http://book.realworldhaskell.org/feeds/comments/).

Copyright 2007, 2008 Bryan O'Sullivan, Don Stewart, and John Goerzen. This work is licensed under a [Creative Commons Attribution-Noncommercial 3.0 License](http://creativecommons.org/licenses/by-nc/3.0/). Icons by [Paul Davey](mailto:mattahan@gmail.com) aka [Mattahan](http://mattahan.deviantart.com/).

[Prev](http://book.realworldhaskell.org/read/systems-programming-in-haskell.html) 

 

 [Next](http://book.realworldhaskell.org/read/extended-example-web-client-programming.html)

Chapter 20. Systems Programming in Haskell 

[Home](http://book.realworldhaskell.org/read/index.html)

 Chapter 22. Extended Example: Web Client Programming

_uacct = "UA-1805907-3"; urchinTracker();
