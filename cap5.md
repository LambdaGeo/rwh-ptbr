(0 Um rápido passeio de JSON)

(0 Neste capítulo, vamos desenvolver, uma biblioteca Haskell pequeno, mas completo. Nossa biblioteca irá manipular e seriar dados de uma forma popular conhecido como JSON.)

(JavaScript Object Notation) language is a small, simple representation for storing and transmitting structured data, for example over a network connection. It is most commonly used to transfer data from a web service to a browser-based JavaScript application. The JSON format is described at www.json.org, and in greater detail by RFC 4627. |#

(1 O (JavaScript Object Notation) língua JSON é uma pequena representação, simples para armazenar e transmitir dados estruturados, por exemplo através de uma conexão de rede. É mais comumente usado para transferir dados de um serviço web para uma aplicação JavaScript baseada em browser. O formato JSON é descrito em (a www.json.org) , E em maior detalhe por (a RFC 4627) .)

(2 JSON suporta quatro tipos básicos de valor: cordas, números, booleanos, e um valor especial chamado (code null) .)

(3 A linguagem fornece dois tipos de compostos: um (span (em array)) é uma seqüência ordenada de valores, e um (span (em object)) é uma coleção desordenada de pares nome / valor. Os nomes em um objeto são sempre cordas; os valores de um objecto ou de matriz pode ser de qualquer tipo.)

(4 Representando dados JSON em Haskell)

(4 Para trabalhar com dados JSON em Haskell, usamos um tipo de dados algébrica para representar a gama de possíveis tipos JSON.)

(5 Para cada tipo de JSON, nós fornecemos um construtor valor distinto. Alguns desses construtores têm parâmetros: se queremos construir uma string JSON, devemos fornecer uma (span String) valor como um argumento para o (code JString) construtor.)

(6 Para começar a experimentar com este código, salve o arquivo (code SimpleJSON.hs) em seu editor, mude para um (span (strong ghci)) janela e carregar o arquivo no (span (strong ghci)) .)

(7 Podemos ver como usar um construtor para assumir um valor Haskell normal e transformá-lo em um (span JValue) . Para fazer o inverso, usamos padrão de correspondência. Aqui está uma função que podemos acrescentar a (code SimpleJSON.hs) que irá extrair uma seqüência de caracteres de um valor JSON para nós. Se o valor JSON, na verdade, contém uma seqüência de caracteres, a nossa função vai envolver a corda com o (code Just) construtor. Caso contrário, ele irá retornar (code Nothing) .)

(The :reload command remembers the last source file we loaded, so we do not need to name it explicitly.) |#

(8 Quando salvar o arquivo de origem modificado, podemos carregá-lo, (span (strong ghci)) e experimentar a nova definição. (A (span (strong :reload)) comando lembra o último arquivo de origem, lotados, por isso, não precisa de nomeá-la explicitamente.))

(9 Mais algumas funções de acesso, e nós temos um pequeno corpo de código para trabalhar com.)

(10 O (code truncate) função transforma um ponto flutuante ou número racional em um número inteiro, largando os dígitos depois do ponto decimal.)

(11 A anatomia de um módulo Haskell)

(11 Um arquivo de origem Haskell contém uma definição de um único (span (em module)) . Um módulo permite-nos determinar quais nomes dentro do módulo podem ser acessados ​​a partir de outros módulos.)

(12 Um arquivo de origem começa com uma (span (em module declaration)) . Isto deve preceder todas as outras definições no arquivo de origem.)

(the component before the suffix) as the name of the module it contains. This is why our file SimpleJSON.hs contains a module named SimpleJSON. |#

(13 A palavra (code module) está reservada. Ele é seguido pelo nome do módulo, que deve começar com uma letra maiúscula. Um arquivo de origem deve ter o mesmo (span (em base name)) (O componente antes do sufixo) como o nome do módulo que contém. É por isso que o nosso arquivo (code SimpleJSON.hs) contém um módulo chamado (code SimpleJSON) .)

(14 Seguindo o nome de módulo é uma lista de (span (em exports)) , Entre parênteses. O (code where) palavra-chave indica que o corpo do módulo seguinte.)

(..) that follows the name JValue indicates that we are exporting both the type and all of its constructors. |#

(15 A lista de exportações indica que nomes neste módulo são visíveis para outros módulos. Isso nos permite manter o código privado escondido do mundo exterior. A notação especial (code (..)) que se segue o nome (code JValue) indica que nós estamos exportando tanto o tipo e todos os seus construtores.)

(i.e. its type constructor), but not its value constructors. The ability to do this is important: it lets us hide the details of a type from its users, making the type abstract. If we cannot see a type's value constructors, we cannot pattern match against a value of that type, nor can we construct a new value of that type. Later in this chapter, we'll discuss some situations in which we might want to make a type abstract. |#

(16 Pode parecer estranho que podemos exportar o nome de um tipo (ou seja, seu construtor tipo), mas não seus construtores de valor. A capacidade de fazer isso é importante: ela nos permite esconder os detalhes de um tipo de seus usuários, fazendo com que o tipo (span (em abstract)) . Se não podemos ver construtores de valor de um tipo, não podemos padrão de jogo contra um valor desse tipo, nem podemos construir um novo valor desse tipo. Mais adiante neste capítulo, discutiremos algumas situações em que se pode querer fazer um tipo abstrato.)

(and the parentheses that enclose them) from a module declaration, every name in the module will be exported. |#

(17 Se omitir as exportações (e os parênteses que eles encerram) a partir de uma declaração do módulo, cada nome no módulo serão exportados.)

(which is rarely useful), we write an empty export list using a pair of parentheses. |#

(18 Para exportar nenhum nome em tudo (que raramente é útil), podemos escrever uma lista de exportação vazia usando um par de parênteses.)

(19 Compilando fonte Haskell)

(the C++ compiler component of Microsoft's Visual Studio), you'll immediately be at home with ghc. |#

(19 Além do (span (strong ghci)) interpretador, o (span GHC) distribuição inclui um compilador, (span (strong ghc)) , Que gera código nativo. Se você já está familiarizado com um compilador de linha de comando como (span (strong gcc)) ou (span (strong cl)) (O componente do compilador C da Microsoft Visual Studio), você estará imediatamente em casa com (span (strong ghc)) .)

(20 Para compilar um arquivo de origem, primeiro abra um terminal ou prompt de comando janela, em seguida, chamar (span (strong ghc)) com o nome do arquivo de origem para compilar.)

(21 O (code -c) opção informa (span (strong ghc)) para gerar somente o código objeto. Se tivéssemos de omitir o (code -c) opção, o compilador iria tentar gerar um executável completa. Isso seria um fracasso, porque não ter escrito uma (code main) função, que (span GHC) chama para iniciar a execução de um programa independente.)

(22 Depois (span (strong ghc)) conclui, se listar o conteúdo do diretório, ele deve conter dois novos arquivos: (code SimpleJSON.hi) e (code SimpleJSON.o) . O primeiro é um (span (em interface file)) , Em que (span (strong ghc)) armazena informações sobre os nomes exportados do nosso módulo em forma legível por máquina. O último é um (span (em object file)) , Que contém o código de máquina gerado.)

(23 Gerando um programa Haskell, e importação de módulos)

(23 Agora que já compilado com sucesso nossa biblioteca mínimo, vamos escrever um pequeno programa para exercê-la. Crie o seguinte arquivo em seu editor de texto e salve-o como (code Main.hs) .)

(24 Observe o (code import) directiva que segue a declaração do módulo. Isso indica que queremos levar todos os nomes que são exportados a partir do (code SimpleJSON) módulo, e disponibilizá-los em nosso módulo. Qualquer (code import) directivas deve aparecer em um grupo no início de um módulo. Eles devem aparecer após a declaração do módulo, mas antes de todos os outros códigos. Não podemos, por exemplo, espalhá-los ao longo de um arquivo de origem.)

(25 Nossa escolha de nomes do arquivo de origem e função é deliberada. Para criar um executável, (span (strong ghc)) espera um módulo chamado (code Main) que contém uma função chamada (code main) . O (code main) função é a que será chamado quando corremos o programa, uma vez que já construiu.)

(26 Desta vez, estamos omitindo o (code -c) opção quando invocamos (span (strong ghc)) , Por isso vai tentar gerar um executável. O processo de geração de um executável é chamada (span (em linking)) . Como a nossa linha de comando sugere, (span (strong ghc)) é perfeitamente capaz de ambos compilar os arquivos de origem e vincular um executável em uma única chamada.)

(27 Passamos (span (strong ghc)) uma nova opção, (code -o) , Que tem um argumento: este é o nome do executável que (span (strong ghc)) deve criar (sup [ (a 10) ]) . Aqui, nós decidimos nomear o programa (code simple) . No Windows, o programa terá o sufixo (code .exe) , Mas em variantes do Unix não haverá um sufixo.)

(28 Finalmente, nós fornecemos o nome do nosso novo arquivo de origem, (code Main.hs) Eo arquivo objeto que já compilado, (code SimpleJSON.o) . Devemos listar explicitamente cada um de nossos arquivos que contém código que deve acabar no executável. Se esquecermos um arquivo fonte ou objeto, (span (strong ghc)) irá queixar-se (span (em undefined symbols)) , O que indica que algumas das definições que necessita não são fornecidos nos ficheiros que forneceram.)

(29 Ao compilar, podemos passar (span (strong ghc)) qualquer mistura de arquivos de origem e de objetos. Se (span (strong ghc)) avisos que já compilados um arquivo de origem em um arquivo objeto, ele só irá recompilar o arquivo de origem se nós modificamos isso.)

(30 Uma vez (span (strong ghc)) terminar de compilar e ligar o nosso (code simple) programa, podemos executá-lo a partir da linha de comando.)

(31 Imprimindo dados JSON)

(31 Agora que temos uma representação Haskell para tipos de JSON, nós gostaríamos de ser capaz de assumir valores Haskell e processá-los como dados JSON.)

(32 Existem algumas formas que poderíamos ir a este respeito. Talvez o mais direto seria escrever uma função de renderização que imprime um valor em forma de JSON. Uma vez que estamos a fazer, vamos explorar algumas abordagens mais interessantes.)

(33 Bom estilo Haskell envolve a separação de código puro de código que realiza I / O. Nossa (code renderJValue) função não tem interação com o mundo exterior, mas ainda precisamos de ser capaz de imprimir uma (span JValue) .)

(34 Imprimindo um valor JSON agora é fácil.)

(35 Por que devemos separar o código de processamento do código que realmente imprime um valor? Isso nos dá flexibilidade. Por exemplo, se quiséssemos para comprimir os dados antes de escrevê-lo para fora, e nós misturados render com a impressão, seria muito mais difícil de se adaptar o nosso código para que a mudança de circunstâncias.)

(36 Esta idéia de separar puro do impuro código é poderoso, e difundida em código Haskell. Existem várias bibliotecas de compressão Haskell, todos os que têm interfaces simples: a função de compressão aceita uma seqüência descompactado e retorna uma string comprimido. Podemos usar composição de função para processar dados JSON para uma cadeia, então comprimir a outra seqüência, adiando qualquer decisão sobre como realmente exibir ou transmitir os dados.)

(37 Inferência de tipos é uma faca de dois gumes)

(37 A capacidade de um compilador Haskell inferir tipos é poderosa e valiosa. Logo no início, você provavelmente vai ser enfrentado por uma forte tentação de aproveitar a inferência de tipos omitindo tantas declarações de tipo quanto possível: vamos simplesmente fazer a figura compilador todo o lote para fora!)

(38 Poupar na informação explícita do tipo tem um lado negativo, que afeta desproporcionalmente novo programador Haskell. Como um novo programador Haskell, estamos extremamente provável que escrever um código que não será compilado devido a erros de tipo simples.)

(39 Quando omitir informações de tipo explícito, forçamos o compilador para descobrir as nossas intenções. Ele vai inferir tipos que são lógica e consistente, mas talvez não o que queríamos dizer. Se o compilador e, sem saber, discordam sobre o que está acontecendo, ele naturalmente vai nos levar mais tempo para encontrar a fonte do nosso problema.)

(40 Suponha, por exemplo, que nós escrevemos uma função que acreditamos retornos uma (span String) , Mas nós não escrever uma assinatura de tipo para ele.)

(41 Aqui, queremos maiúscula o primeiro caractere de uma palavra, mas se esqueceu de acrescentar o resto da palavra sobre o resultado. Pensamos tipo da nossa função é (span String -&gt; String) , Mas o compilador inferir corretamente seu tipo (span String -&gt; Char) . Digamos que, em seguida, tentar usar esta função em outro lugar.)

(42 Quando tentamos compilar este código ou carregá-lo em (span (strong ghci)) , Não necessariamente uma mensagem de erro óbvio.)

(43 Observe que o erro é relatado onde nós (span (em use)) o (code upcaseFirst) função. Se estamos erroneamente convencido de que a nossa definição e tipo de (code upcaseFirst) estão corretas, podemos acabar olhando para a peça errada do código por um bom tempo, até que as greves de esclarecimento.)

(44 Cada vez que gravar uma assinatura tipo, nós removemos um grau de liberdade a partir do motor de inferência de tipos. Isso reduz a probabilidade de divergência entre a nossa compreensão do nosso código e o compilador do. As declarações de tipo também atuam como abreviação para nós mesmos como leitores do nosso próprio código, tornando mais fácil para nós para desenvolver uma noção do que deve estar acontecendo.)

(45 Isso não quer dizer que precisamos de pimenta cada pequeno fragmento de código com uma declaração de tipo. É, no entanto, geralmente boa forma de adicionar uma assinatura a cada definição de nível superior no nosso código. É melhor começar bastante agressivo com assinaturas de tipo explícitas, e, lentamente, a facilidade de volta como o seu modelo mental de como funciona a verificação de tipo torna-se mais precisa.)

(46 O valor especial (code undefined) terá todo o prazer typecheck não importa onde vamos usá-lo, assim como uma expressão como (code error "argh!") . É especialmente importante que nós escrevemos tipo assinaturas quando usamos estes. Suponha que nós usamos (code undefined) ou (code error "write me") para actuar como um marcador de posição no corpo de uma definição de nível superior. Se omitir a assinatura de tipo, que pode ser capaz de usar o valor que definimos em locais onde uma versão corretamente digitado seria rejeitado pelo compilador. Isso pode facilmente levar-nos ao erro.)

(47 Um olhar mais geral na renderização)

(47 Nosso código de renderização JSON é estritamente sob medida para as necessidades exatas de nossos tipos de dados e as convenções de formatação JSON. A saída que produz pode ser hostil aos olhos humanos. Vamos agora olhar para render como uma tarefa mais genérico: como podemos construir uma biblioteca que é útil para tornar os dados em uma variedade de situações?)

(e.g. for debugging) or for machine processing. Libraries that perform this job are referred to as pretty printers. There already exist several Haskell pretty printing libraries. We are creating one of our own not to replace them, but for the many useful insights we will gain into both library design and functional programming techniques. |#

(48 Nós gostaríamos de produzir uma saída que é adequado tanto para o consumo humano (por exemplo, para depuração) ou para o processamento da máquina. Bibliotecas que realizam este trabalho são referidos como (span (em pretty printers)) . Já existem várias bibliotecas de impressão bastante Haskell. Estamos criando um dos nossos não para substituí-los, mas para os muitos conhecimentos úteis que vai ganhar em ambas projeto biblioteca e técnicas de programação funcional.)

(49 Ligamos o nosso módulo de impressão bastante genérico (code Prettify) , Então o nosso código vai entrar em um arquivo de origem nomeado (code Prettify.hs) .)

(50 Naming)

(51 No nosso (code Prettify) módulo, vamos basear os nossos nomes nos instrumentos usados ​​por várias bibliotecas de impressão bonitas Haskell estabelecida. Isso nos dará um grau de compatibilidade com bibliotecas maduros existentes.)

(52 Para certificar-se de que (code Prettify) atende às necessidades concretas, é escrever um novo renderizador JSON que usa o (code Prettify) API. Depois que estiver pronto, nós vamos voltar e preencher os detalhes do (code Prettify) módulo.)

(53 Em vez de tornar reta a uma corda, o nosso (code Prettify) módulo irá usar um tipo abstrato que chamaremos (span Doc) . Ao basear a nossa biblioteca de renderização genérica sobre um tipo abstrato, podemos escolher uma implementação que é flexível e eficiente. Se decidirmos mudar o código subjacente, nossos usuários não serão capazes de dizer.)

(54 Vamos citar o nosso novo módulo de renderização JSON (code PrettyJSON.hs) E manter o nome (code renderJValue) para a função de renderização. Renderização de um dos valores básicos JSON é simples.)

(55 O (code text) , (code double) E (code string) funções serão fornecidos pelo nosso (code Prettify) módulo.)

(56 Desenvolver código Haskell sem enlouquecendo)

(56 Logo no início, como nós vir a enfrentar desenvolvimento Haskell, temos tantos, novos conceitos desconhecidos para acompanhar de uma vez que ele pode ser um desafio para escrever código que compila em tudo.)

(57 Enquanto escrevemos o nosso primeiro conjunto substancial de código, é uma (span (em huge)) ajudar a fazer uma pausa a cada poucos minutos e tentar compilar o que temos produzido até agora. Porque Haskell é tão fortemente digitado, se o nosso código compila de forma limpa, estamos assegurando-nos de que não estamos vagando muito longe para as ervas daninhas de programação.)

(58 Uma técnica útil para desenvolver rapidamente o esqueleto de um programa é escrever espaço reservado, ou (span (em stub)) versões de tipos e funções. Por exemplo, nós mencionamos acima que a nossa (code string) , (code text) e (code double) funções seria fornecido por nossa (code Prettify) módulo. Se não fornecer definições para essas funções ou o (span Doc) tipo, nossas tentativas de \x26quot; (span compile early, compile often) \x26quot;Com a nossa representante JSON irá falhar, como o compilador não sabe nada sobre essas funções. Para evitar esse problema, é escrever o código stub que não faz nada.)

(59 O valor especial (code undefined) tem o tipo de (code a) , Por isso sempre typechecks, não importa onde vamos usá-lo. Se tentarmos para avaliá-lo, ele fará com que o nosso programa trave.)

(60 Mesmo que ainda não é possível executar o nosso código apagou, tipo verificador do compilador irá garantir que o nosso programa é sensivelmente digitado.)

(61 Consideravelmente imprimir uma string)

(61 Quando devemos muito imprimir um valor de cadeia, JSON tem moderadamente envolvido escapar regras que devemos seguir. No nível mais alto, uma string é apenas uma série de personagens envolvidos em aspas.)

(62 Estilo livre-Point)

(in Haskell) with value, so a point-free expression makes no mention of the values that it operates on. |#

(63 Este estilo de escrever uma definição exclusivamente como uma composição de outras funções é chamado (span (em point-free style)) . A utilização da palavra \x26quot; (span point) \x26quot;Não está relacionada com a\x26quot; (span (code .)) \x26quot;Caráter utilizado para a composição da função. O termo (span (em point)) é praticamente sinônimo (em Haskell) com (span (em value)) , Portanto, um (span (em point-free)) expressão não faz nenhuma menção dos valores que atua diante.)

(64 Contraste a definição livre de ponto de (code string) acima com este \x26quot; (span pointy) \x26quot;Versão, que utiliza uma variável (code s) para se referir ao valor em que atua.)

(65 O (code enclose) função simplesmente envolve uma (span Doc) valor com uma abertura e fechamento de caráter.)

(&lt;&gt;) function in our pretty printing library. It appends two Doc values, so it's the Doc equivalent of (++). |#

(66 Nós fornecemos uma (code (&lt;&gt;)) função em nossa biblioteca de impressão bonita. Ele acrescenta dois (span Doc) valores, por isso é o (span Doc) equivalente de (code (++)) .)

(67 Nossa biblioteca impressão bastante também fornece (code hcat) , Que encadeia múltipla (span Doc) valores em um: é o análogo (code concat) para listas.)

(68 Nossa (code string) função aplica-se a (code oneChar) função para cada caractere em uma string, concatena o lote, e encerra o resultado entre aspas. O (code oneChar) função escapa ou torna um caráter individual.)

(69 O (code simpleEscapes) valor é uma lista de pares. Chamamos uma lista de pares um (span (em association list)) Ou (span (em alist)) para breve. Cada elemento da nossa alist associa um personagem com a sua representação escapou.)

(70 Nossa (code case) expressão tenta ver se o nosso personagem tem uma partida neste alist. Se encontrarmos a partida, nós emiti-lo, caso contrário, pode precisar escapar do personagem de uma maneira mais complicada. Se assim for, nós realizamos este escapar. Só se for necessária nem tipo de escapar do que emitimos o caráter simples. Para ser conservador, os únicos personagens unescaped que emitimos são caracteres ASCII imprimíveis.)

(71 O escape mais complicado envolve virando um personagem para a string \x26quot; (span (code \u)) \x26quot;Seguido por uma seqüência de quatro caracteres de dígitos hexadecimais que representam o valor numérico do caráter Unicode.)

(you will need to import this at the beginning of Prettify.hs), and returns a hexadecimal representation of a number. |#

(72 O (code showHex) função vem do (code Numeric) biblioteca (você terá de importar este no início de (code Prettify.hs) ), E retorna uma representação hexadecimal de um número.)

(73 O (code replicate) função é fornecida pelo Prelude, e cria uma lista de repetição de comprimento fixo de seu argumento.)

(74 Há uma ruga: a codificação de quatro dígitos que (code smallHex) fornece só pode representar caracteres Unicode até (code 0xffff) . Caracteres Unicode válidos pode variar até (code 0x10ffff) . Para representar adequadamente um personagem acima (code 0xffff) em uma string JSON, seguimos algumas regras complicadas para dividi-lo em dois. Isto dá-nos uma oportunidade para realizar alguma manipulação de nível pouco de números Haskell.)

(.&amp;.) function, also from Data.Bits, performs a bit-level and of two values. |#

(75 O (code shiftR) função vem do (code Data.Bits) módulo, e desloca um número à direita. O (code (.&amp;.)) função, também a partir de (code Data.Bits) , Realiza um nível pouco (span (em and)) de dois valores.)

(76 Agora que temos escrito (code smallHex) e (code astral) , Podemos fornecer uma definição para (code hexEscape) .)

(77 Arrays e objetos, e o cabeçalho do módulo)

(77 Em comparação com cordas, matrizes de impressão bonitas e objetos é uma pressão. Nós já sabemos que os dois são visualmente semelhantes: cada um começa com um caráter de abertura, seguido por uma série de valores separados por vírgulas, seguido de um caractere de fechamento. Vamos escrever uma função que captura a estrutura comum de matrizes e objetos.)

(78 Vamos começar pela interpretação deste tipo de função. É preciso uma abertura e fechamento personagem, em seguida, uma função que sabe muito imprimir um valor de algum tipo desconhecido (code a) , Seguida por uma lista de valores de tipo (code a) E retorna um valor do tipo (span Doc) .)

(79 Note-se que embora a nossa assinatura de tipo menciona quatro parâmetros, só listei três na definição da função. Estamos simplesmente seguindo a mesma regra que nos permite simplificar a definiton como (code myLength xs = length xs) para (code myLength = length) .)

(80 Nós já escrevemos (code enclose) , Que envolve uma (span Doc) valor na abertura e fechamento de caracteres. O (code fsep) função irá viver em nossa (code Prettify) módulo. Ele combina uma lista de (span Doc) valores em um, possivelmente envolvendo linhas se a saída não vai caber em uma única linha.)

(81 Até agora, você deve ser capaz de definir seus próprios tocos em (code Prettify.hs) , Seguindo os exemplos que fornecemos. Nós não vamos definir explicitamente qualquer mais tocos.)

(82 O (code punctuate) função também vai viver em nossos (code Prettify) módulo, e podemos defini-lo em termos de funções para as quais nós já tocos escritas.)

(83 Com esta definição de (code series) , Consideravelmente a impressão de uma matriz é inteiramente clara. Eu acrescento esta equação para o final do bloco que já escreveu para o nosso (code renderJValue) função.)

(84 Para consideravelmente imprimir um objeto, o que precisamos fazer apenas um pouco mais de trabalho: para cada elemento, temos um nome e um valor de lidar.)

(85 Escrevendo um cabeçalho do módulo)

(85 Agora que temos escrito a maior parte do nosso (code PrettyJSON.hs) arquivo, temos de voltar ao topo e adicionar uma declaração módulo.)

(86 Nós exportamos apenas um nome a partir deste módulo: (code renderJValue) , Nossa JSON função de renderização. As outras definições no módulo existe puramente para apoio (code renderJValue) , Então não há nenhuma razão para torná-los visíveis para outros módulos.)

(87 Em relação às importações, a (code Numeric) e (code Data.Bits) módulos são distribuídos com (span GHC) . Nós já escreveu o (code SimpleJSON) módulo, e encheu o nosso (code Prettify) módulo com as definições do esqueleto. Note-se que não há diferença na forma como importar módulos padrão daqueles que temos de nós mesmos por escrito.)

(88 Com cada (code import) directiva, que explicitamente listar cada um dos nomes que queremos trazer para namespace do nosso módulo. Isso não é necessário: se omitir a lista de nomes, todos os nomes exportados a partir de um módulo estará disponível para nós. No entanto, é geralmente uma boa idéia para escrever uma lista de importação explícita.)

(89 Uma lista explícita deixa claro que nomes que estamos importando de onde. Isso fará com que seja mais fácil para o leitor a olhar para cima documentação se depararem com uma função desconhecida.)

(90 Ocasionalmente, um mantenedor biblioteca irá remover ou mudar o nome de uma função. Se uma função desaparece a partir de um terceiro módulo partido que usamos, qualquer erro de compilação resultante é provável de acontecer muito tempo depois que nós escrevemos o módulo. A lista explícita de nomes importados pode agir como um lembrete para nós de onde tínhamos vindo a importar o nome faltando, o que vai nos ajudar a identificar o problema mais rapidamente.)

(91 Também pode ocorrer que alguém vai acrescentar um nome a um módulo que é idêntico a um nome já em nosso próprio código. Se não usar uma lista de importação explícita, vamos acabar com o mesmo nome em nosso módulo duas vezes. Se usarmos esse nome, (span GHC) indicará um erro devido à ambigüidade. Uma lista explícita nos permite evitar a possibilidade de, acidentalmente, a importação de um novo nome inesperado.)

(92 Esta ideia de utilizar as importações explícitas é uma diretriz que normalmente faz sentido, não uma regra dura e rápida. Ocasionalmente, vamos precisar de tantos nomes de um módulo que listando cada um torna-se confuso. Em outros casos, um módulo pode ser tão amplamente utilizado que um programador Haskell moderada experiência irá provavelmente sabe que nomes vêm desse módulo.)

(93 Concretizar a biblioteca impressão bonita)

(93 No nosso (code Prettify) módulo, nós representamos o nosso (span Doc) digite como um tipo de dados algébrica.)

(94 Observa-se que o (span Doc) tipo é realmente uma árvore. O (code Concat) e (code Union) construtores criar um nó interno a partir de dois outros (span Doc) valores, enquanto o (code Empty) e outros construtores simples construir folhas.)

(95 No cabeçalho da nossa módulo, vamos exportar o nome do tipo, mas não qualquer dos seus construtores: isso vai impedir que os módulos que utilizam o (span Doc) tipo de criação e padrão de correspondência contra (span Doc) valores.)

(96 Em vez disso, para criar uma (span Doc) , Um utilizador do (code Prettify) módulo irá chamar uma função que nós fornecemos. Aqui estão as funções de construção simples. À medida que adicionamos definiçes reais, devemos substituir quaisquer versões stubados já no (code Prettify.hs) arquivo de origem.)

(97 O (code Line) constructor representa uma quebra de linha. O (code line) função cria (span (em hard)) quebras de linha, que sempre aparecem na saída da impressora bonita. Às vezes a gente vai querer um (span (em soft)) quebra de linha, o que só é usado se a linha for muito grande para caber em uma janela ou página. Vamos introduzir um (code softline) função em breve.)

(&lt;&gt;) function, which concatenates two Doc values. |#

(98 Quase tão simples como os construtores básicos é o (code (&lt;&gt;)) função, que concatena dois (span Doc) valores.)

(99 Nós jogo padrão contra (code Empty) de modo que uma concatenação (span Doc) valor com (code Empty) à esquerda ou à direita não terá efeito. Isso nos impede de inchaço da árvore com valores inúteis.)

(100 Se nós brevemente colocar em nossos chapéus matemáticas, podemos dizer que (code Empty) é a identidade sob a concatenação, já que nada acontece se concatenar uma (span Doc) valor com (code Empty) . Na mesma linha, 0 é a identidade para a adição de números, e 1 é a identidade para multiplicá-los. Tomando a perspectiva matemática tem consequências práticas úteis, como veremos em vários lugares ao longo deste livro.)

(101 Nossa (code hcat) e (code fsep) funções concatenar uma lista de (span Doc) valores em um. Em (a the section called “Exercises”) , Mencionamos que poderíamos definir concatenação de listas usando (code foldr) .)

(&lt;&gt;) is analogous to (++), and empty to [], we can see how we might write hcat and fsep as folds, too. |#

(102 Desde (code (&lt;&gt;)) é análogo ao (code (++)) E (code empty) para (code []) , Podemos ver como podemos escrever (code hcat) e (code fsep) como sulcos, também.)

(103 A definição de (code fsep) depende de várias outras funções.)

(104 Estes têm um pouco de explicação. O (code softline) função deve inserir uma nova linha se a linha atual tornou-se muito grande, ou um espaço de outra forma. Como podemos fazer isso se o nosso (span Doc) tipo não contém qualquer informação sobre o processamento? A nossa resposta é que cada vez que nos deparamos com uma nova linha suave, mantemos (span (em two)) representações alternativas do documento, utilizando o (code Union) construtor.)

(105 Nossa (code flatten) função substitui um (code Line) com um espaço, transformando duas linhas em uma linha mais longa.)

(in characters) as, or wider than, the right. We'll be making use of this property in our rendering functions below. |#

(106 Observe que nós sempre chamamos (code flatten) sobre o elemento esquerdo de um (code Union) : À esquerda de cada (code Union) é sempre a mesma largura (em caracteres) como, ou mais larga do que, à direita. Nós estaremos fazendo uso desta propriedade em nossas funções de renderização abaixo.)

(107 Renderização Compact)

(107 Nós freqüentemente precisam usar uma representação de uma peça de dados que contém o mínimo de caracteres possível. Por exemplo, se estamos enviando dados JSON através de uma conexão de rede, não há nenhum sentido em colocar-lo bem: o software no extremo não vai se importar se os dados são bonitas ou não, bem como o espaço em branco adicional necessária para fazer a boa aparência de layout gostaria de acrescentar um monte de sobrecarga.)

(108 Para esses casos, e porque é um simples pedaço de código para começar, nós fornecemos uma função de renderização compacto nu-ossos.)

(109 O (code compact) função envolve seu argumento em uma lista, e aplica o (code transform) função de auxiliar a ele. O (code transform) função trata seu argumento como uma pilha de artigos a processar, em que o primeiro elemento da lista é o topo da pilha.)

(d:ds) pattern breaks the stack into its head, d, and the remainder, ds. In our case expression, the first several branches recurse on ds, consuming one item from the stack for each recursive application. The last two branches add items in front of ds: the Concat branch adds both elements to the stack, while the Union branch ignores its left element, on which we called flatten, and adds its right element to the stack. |#

(110 O (code transform) função de (code (d:ds)) padrão de quebra a pilha em sua cabeça, (code d) , E o restante, (code ds) . No nosso (code case) expressão, o primeiro diversos ramos recursividade em (code ds) , Consumindo um item da pilha para cada aplicação recursiva. Os dois últimos ramos adicionar itens em frente (code ds) : O (code Concat) ramo adiciona ambos os elementos para a pilha, enquanto o (code Union) ramo ignora seu elemento esquerdo, no qual chamamos (code flatten) , E adiciona o seu direito de o elemento de pilha.)

(111 Temos agora concretizada suficiente de nossas definições esqueléticos originais que podemos experimentar a nossa (code compact) função em (span (strong ghci)) .)

(112 Para melhor compreender a forma como o código funciona, vamos olhar para um exemplo mais simples de forma mais detalhada.)

(113 Quando aplicamos (code compact) , Verifica-se o seu argumento em uma lista e aplica (code transform) .)

(d:ds) pattern. Thus d is the value Concat (Char 'f') (Text "oo"), and ds is the empty list, []. |#

(114 O (code transform) função recebe uma lista de um único item, o que corresponde a (code (d:ds)) padrão. Assim (code d) é o valor (code Concat (Char 'f') (Text "oo")) E (code ds) é a lista vazia, (code []) .)

(115 Desde (code d) \x26#39;S construtor é (code Concat) , O (code Concat) padrão corresponde no (code case) expressão. No lado direito, nós adicionamos (code Char 'f') e (code Text "oo") para a pilha, e aplicar (code transform) recursivamente.)

(d:ds) pattern. The variable d is bound to Char 'f', and ds to [Text "oo"]. |#

(116 O (code transform) função recebe uma lista de dois itens, mais uma vez combinando o (code (d:ds)) padrão. A variável (code d) é obrigado a (code Char 'f') E (code ds) para (code [Text "oo"]) .)

(:) to construct a list whose head is 'f', and whose body is the result of a recursive application of transform. |#

(117 O (code case) expressão corresponde no (code Char) galho. No lado direito, nós usamos (code (:)) para a construção de uma lista cuja cabeça é (code 'f') , E cujo corpo é o resultado de uma aplicação de recursiva (code transform) .)

(118 A invocação recursiva recebe uma lista de um único item. A variável (code d) é obrigado a (code Text "oo") E (code ds) para (code []) .)

(++) to concatenate "oo" with the result of a recursive application of transform. |#

(119 O (code case) expressão corresponde no (code Text) galho. No lado direito, nós usamos (code (++)) concatenar (code "oo") com o resultado de uma aplicação de recursiva (code transform) .)

(120 Na invocação final, (code transform) é chamado com uma lista vazia, e retorna uma cadeia vazia.)

(121 O resultado é (code "oo" ++ "") .)

(122 O resultado é (code 'f' : "oo" ++ "") .)

(123 Impressão bastante fiel)

(We're assuming that our typeface is of fixed width.) |#

(123 Enquanto a nossa (code compact) função é útil para a comunicação máquina-a-máquina, o seu resultado nem sempre é fácil para um ser humano a seguir: há muito pouca informação sobre cada linha. Para gerar a saída mais legível, vamos escrever outra função, (code pretty) . Comparado com (code compact) , (code pretty) tem um argumento extra: a largura máxima de uma linha, em colunas. (Estamos supondo que a nossa fonte é de largura fixa.))

(124 Para ser mais preciso, este (span Int) parâmetro controla o comportamento do (code pretty) quando encontra um (code softline) . Só numa (code softline) faz (code pretty) tem a opção de continuar a linha atual ou começando uma nova linha. Em outros lugares, devemos seguir rigorosamente as diretrizes estabelecidas pela pessoa que utiliza os nossos funções de impressão bonitas.)

(125 Aqui está o cerne da nossa implementação)

(126 Nossa (code best) função auxiliar recebe dois argumentos: o número de colunas emitidos até agora na linha atual, e a lista de remanescentes (span Doc) valores a serem processados.)

(127 Nos casos simples, (code best) atualiza o (code col) variável de formas simples como ele consome de entrada. Mesmo o (code Concat) caso é óbvia: nós empurramos os dois componentes concatenados na nossa pilha / list, e não toque (code col) .)

(if either) of the two layouts, the flattened one or the original, will fit into our width restriction. |#

(128 O caso interessante envolve a (code Union) construtor. Lembre-se que nós aplicamos (code flatten) para o elemento de esquerda, e não fez nada para a direita. Além disso, lembre-se que (code flatten) substitui novas linhas com espaços. Portanto, nosso trabalho é ver que (se tanto) dos dois layouts, o (code flatten) ed um ou o original, se encaixa em nosso (code width) restrição.)

(129 Para fazer isso, nós escrevemos um pequeno ajudante que determina se uma única linha de um prestados (span Doc) valor se encaixam em um determinado número de colunas.)

(130 Após a impressora bonita)

(130 A fim de compreender como esse código funciona, vamos primeiro considerar uma simples (span Doc) valor.)

(Char ' ') Line and Char 'a' onto the stack, and applies itself recursively. In the recursive application, it matches on Union (Char ' ') Line. |#

(131 Vamos aplicar (code pretty 2) sobre este valor. Quando começamos a aplicar (code best) , O valor de (code col) é zero. Ela corresponde a (code Concat) caso, empurra os valores (code Union (Char ' ') Line) e (code Char 'a') para a pilha, e se aplica de forma recursiva em si. Na aplicação recursiva, ele corresponde a (code Union (Char ' ') Line) .)

(132 Neste ponto, vamos ignorar ordem habitual de Haskell de avaliação. Isso mantém a nossa explicação sobre o que está acontecendo simples, sem alterar o resultado final. Temos agora duas subexpressions, (code best 0 [Char ' ', Char 'a']) e (code best 0 [Line, Char 'a']) . O primeiro avalia a (code " a") , E para a segunda (code "\na") . Nós, então, substituir estes na expressão exterior para dar (code nicest 0 " a" "\na") .)

(133 Para descobrir qual é o resultado de (code nicest) está aqui, nós fazemos um pouco de substituição. Os valores de (code width) e (code col) são 0 e 2, respectivamente, assim (code least) é 0, e (code width - least) 2. Nós é avaliar rapidamente (code 2 `fits` " a") em (span (strong ghci)) .)

(134 Uma vez que este é avaliada como (code True) , O resultado de (code nicest) aqui é (code " a") .)

(135 Se aplicarmos o nosso (code pretty) função para os mesmos dados JSON como antes, podemos ver que ela produz uma saída diferente, dependendo da largura que damos a ela.)

(136 Exercícios)

(136 Nossa impressora bastante atual é espartano, de modo que vai se encaixar dentro de nossas limitações de espaço, mas há uma série de melhorias úteis que podemos fazer.)

(137 Escreva uma função, (code fill) , Com a seguinte assinatura de tipo.)

(138 Deve adicionar espaços a um documento até que seja dado o número de colunas de largura. Se é já mais larga do que este valor, que deve adicionar nenhum espaços.)

(139 Nosso muito impressora não tomar (span (em nesting)) em conta. Sempre que abrir parênteses, chaves ou colchetes, todas as linhas que se seguem deve ser recuado para que eles estejam alinhados com o personagem de abertura até que um caractere de fechamento correspondente for encontrado.)

(140 Adicionar suporte para o assentamento, com uma quantidade controlável de recuo.)

(141 Criando um pacote)

(141 A comunidade Haskell construiu um conjunto padrão de ferramentas, chamado Cabal, que ajuda com a construção, instalação e distribuição de software. Cabal organiza software como um (span (em package)) . Um pacote contém uma biblioteca, e, eventualmente, vários programas executáveis.)

(142 Escrever uma descrição do pacote)

(142 Para fazer qualquer coisa com um pacote, Cabal precisa de uma descrição do mesmo. Isso está contido em um arquivo de texto cujo nome termina com o sufixo (code .cabal) . Esta imagem pertence no diretório de nível superior do seu projeto. Ele tem um formato simples, que iremos descrever abaixo.)

(143 Um pacote de Cabal deve ter um nome. Normalmente, o nome do pacote coincide com o nome do (code .cabal) arquivo. Vamos chamar nosso pacote (code mypretty) , Então o nosso arquivo é (code mypretty.cabal) . Muitas vezes, o diretório que contém um (code .cabal) arquivo terá o mesmo nome que o pacote, e.g. (code mypretty) .)

(144 A descrição do pacote começa com uma série de propriedades globais, que se aplicam a cada biblioteca e executável no pacote.)

(145 Os nomes de pacotes deve ser exclusivo. Se você criar e instalar um pacote que tem o mesmo nome de um pacote já presente em seu sistema, (span GHC) vai ficar muito confuso.)

(146 As propriedades globais incluem uma quantidade substancial de informação que é destinado a leitores humanos, não Cabal si.)

(147 Como o (code Description) campo indica, um campo pode abranger várias linhas, desde que eles são recuadas.)

(Obviously, you're free to choose whatever license you think is appropriate.) The optional License-File field lets us specify the name of a file that contains the exact text of our package's licensing terms. |#

(148 Também estão incluídos nas propriedades globais é a informação de licença. A maioria dos pacotes Haskell está licenciado sob a licença BSD, que chama Cabal (code BSD3) (sup [ (a 11) ]) . (Obviamente, você é livre para escolher o que quer que licença você acha que é apropriado.) O opcional (code License-File) campo nos permite especificar o nome de um arquivo que contém o texto exato de termos de licenciamento do nosso pacote.)

(149 Os recursos compatíveis com versões sucessivas de Cabal evoluir ao longo do tempo, por isso é aconselhável para indicar quais versões do Cabal esperamos ser compatível com. Os recursos que estamos descrevendo são suportados por versões 1.2 e superiores do Cabal.)

(150 Para descrever uma biblioteca individual dentro de um pacote, nós escrever uma (span (em library section)) . O uso de recuo aqui é significativa: o conteúdo de uma seção deve ser recuado.)

(151 O (code Exposed-Modules) campo contém uma lista de módulos que devem estar disponíveis para os utilizadores deste pacote. Um campo opcional, (code Other-Modules) , Contém uma lista de (span (em internal)) módulos. Estes são necessários para a função desta biblioteca, mas não será visível para os utilizadores.)

(152 O (code Build-Depends) campo contém uma lista de pacotes que nossa biblioteca necessita para construir separados por vírgulas. Para cada pacote, que pode, opcionalmente, especificar o intervalo de versões com que esta biblioteca é conhecido por trabalhar. O (code base) pacote contém muitos dos módulos Haskell centrais, como o Prelude, por isso é efetivamente sempre necessária.)

(153 Não temos de adivinhar ou fazer qualquer investigação para estabelecer quais os pacotes que dependem. Se tentarmos construir o nosso pacote sem um (code Build-Depends) campo, a compilação irá falhar com uma mensagem de erro útil. Aqui está um exemplo onde nós comentou a dependência do (code base) pacote.)

(154 A mensagem de erro deixa claro que nós precisamos adicionar o (code base) pacote, embora (code base) já está instalado. Forçando-nos a ser explícito sobre todos os pacotes que precisamos tem um benefício prático: uma ferramenta de linha de comando chamado (code cabal-install) irá automaticamente baixar, construir e instalar um pacote e todos os pacotes que depende.)

(155 Gerenciador de pacotes do GHC)

(155 (span GHC) inclui um gerenciador de pacotes simples que rastreia quais pacotes são instalados, e que as versões desses pacotes são. A ferramenta de linha de comando chamado (span (strong ghc-pkg)) permite-nos trabalhar com os seus bancos de dados de pacotes.)

(156 Dizemos (span (em databases)) porque (span GHC) distingue entre (span (em system-wide)) pacotes, que estão disponíveis para todos os usuários, e (span (em per-user)) pacotes, que são visíveis apenas para o usuário atual. O banco de dados por usuário nos permite evitar a necessidade de privilégios administrativos para instalar pacotes.)

(We will have to manually delete the installed files ourselves.) |#

(157 O (span (strong ghc-pkg)) comando fornece subcomandos para abordar diferentes tarefas. Na maioria das vezes, só vai precisar de dois deles. O (code ghc-pkg list) comando permite-nos ver que pacotes são instalados. Quando queremos desinstalar um pacote, (code ghc-pkg unregister) diz (span GHC) que não vamos estar a utilizar um determinado pacote por mais tempo. (Nós vamos ter que apagar manualmente os arquivos instalados nós mesmos.))

(158 Configurando, construindo e instalando)

(158 Além de um (code .cabal) arquivo, um pacote deve conter um (span (em setup)) arquivo. Isso permite que processo de construção do Cabal a ser fortemente personalizado, se um pacote precisa. O arquivo de instalação mais simples parece com isso.)

(159 Nós salve este arquivo com o nome (code Setup.hs) .)

(160 Uma vez que temos o (code .cabal) e (code Setup.hs) arquivos escrito, temos três etapas restantes.)

(161 Instruir Cabal como construir e onde instalar o pacote, nós executar um comando simples.)

(162 Isso garante que os pacotes que precisa estão disponíveis, e as configurações de lojas para ser usado mais tarde por outros comandos Cabal.)

(163 Se não fornecer quaisquer argumentos para (code configure) , Cabal irá instalar nosso pacote no banco de dados do pacote de todo o sistema. Para instalá-lo em nosso diretório casa e nosso banco de dados pacote de pessoal, temos de fornecer um pouco mais de informação.)

(164 Seguindo o (code configure) etapa, vamos construir o pacote.)

(165 Se isso tiver êxito, podemos instalar o pacote. Nós não precisamos de indicar onde instalar a: Cabal usará as configurações de nós fornecidos no (code configure) etapa. Ele irá instalar em nosso próprio diretório e atualização (span GHC) \x26quot;Banco de dados s por usuário do pacote.)

(166 Ponteiros práticos e mais leitura)

(166 (span GHC) já empacota uma biblioteca impressão bonito, (code Text.PrettyPrint.HughesPJ) . Ele oferece a mesma API básico como o nosso exemplo, mas um conjunto muito mais rica e útil de funções de impressão bonitas. Recomendamos usá-lo, ao invés de escrever o seu próprio.)

(167 A concepção do (code HughesPJ) pretty printer foi introduzido por John Hughes em [ (span (a (abbr Hughes95))) ]. A biblioteca foi posteriormente melhorado por Simon Peyton Jones, daí o nome. O trabalho de Hughes é longa, mas vale a pena ler para sua discussão sobre como projetar uma biblioteca em Haskell.)

(168 Neste capítulo, a biblioteca de impressão bastante baseia-se num sistema mais simples descrita por Philip Wadler em [ (span (a (abbr Wadler98))) ]. Sua biblioteca foi prorrogado por Daan Leijen; Esta versão está disponível para download a partir Hackage como (code wl-pprint) . Se você usar o (span (strong cabal)) ferramenta de linha de comando, você pode baixar, construir, e instalá-lo em uma etapa com (span (strong cabal install wl-pprint)) .)
