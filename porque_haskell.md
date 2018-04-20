---
# You don't need to edit this file, it's empty on purpose.
# Edit theme's home layout instead if you wanna make some changes
# See: https://jekyllrb.com/docs/themes/#overriding-theme-defaults
layout: page
---
Uma tradução não oficial do livro Real World Haskell 
de Bryan O'Sullivan, Don Stewart, and John Goerzen
-------------------------------------------------------


## Por quê programação funcional? Por quê Haskell?

### Temos uma solução para você!

Haskell é uma linguagem com muitos recursos, e seu aprendizado é uma experiência extremamente gratificante. Vamos nos concentrar em três elementos. O ”primeiro é a _inovação_: nós convidamos você a pensar sobre a programação de uma perspectiva diferente e valiosa. O segundo é o _poder_: nós vamos mostrar-lhe como criar um software que é curto, rápido e seguro. Por fim, oferecemos-lhe um muita _diversão_: o prazer de aplicação de soluções de programação elegantes para resolver problemas reais.

#### Inovadora

Haskell é provavelmente muito diferente do que qualquer linguagem que você já usou antes. Programação funcional nos oferece uma maneira profundamente diferente de pensar sobre softwares.

Em Haskell, nós deixamos de enfatizar códigos que modifica dados. Em vez disso, vamos nos concentrar nas funções que têm valores imutáveis como entrada e produzem novos valores como saída. Dadas as mesmas entradas, estas funções sempre retornam os mesmos resultados. Essa é uma idéia central por trás da programação funcional.

Junto com a não modificação de dados, nossas funções Haskell normalmente, não “dialoga” com o mundo externo, chamamos-as de funções _puras_. Nós fazemos uma distinção forte entre o código puro e as partes dos nossos programas que lêem ou escrevem arquivos, comunicam-se por meio de conexões de rede, ou fazem mover os braços de um robô. Isso torna mais fácil organizar, estudar e testar os nossos programas.

Abandonamos algumas idéias que podem parecer fundamentais, tais como ter um `for` loop incorporadas a linguagem. Temos outras formas mais flexíveis de executar tarefas repetitivas.

Mesmo a maneira pela qual avaliamos as expressões é diferente em Haskell. Nós adiamos todos os cálculos até que seu resultado seja realmente necessário: Haskell é uma linguagem que tem avalição _preguiçosa_. Esse recurso não é apenas um modo de adiar a avaliação: ela afeta profundamente a forma como escrevemos os programas.

### Poderosa

Ao longo deste livro, vamos mostrar a vocês que as alternativas do Haskell para os recursos de linguagens tradicionais são poderosas, flexíveis, e levam a códigos mais confiáveis

Como o código puro não pode ter relações com o mundo exterior, e os dados que ele trabalha nunca são modificados, é muito raro você ter o tipo de surpresa desagradável onde uma parte de um código invisível corrompe os dados utilizados por outro. Qualquer contexto, usamos uma função pura, ele irá se comportar de forma consistente.

Código puro é mais fácil de testar que o código que lida com o mundo exterior. Quando uma função só responde a suas entradas visíveis, podemos afirmar com facilidade as propriedades do seu comportamento que deve ser sempre verdadeiro. Podemos testar automaticamente se essas propriedades se mantem dado um conjunto enorme de entradas aleatórias, e quando passar em nossos testes, podemos seguir em frente acoplando essa função ao sistema como todo. Temos ainda a utilização de técnicas tradicionais para testar o código que deve interagir com arquivos, redes, hardware ou exóticos. Desde há muito menos deste código impura do que se poderia encontrar em uma linguagem tradicional, nós temos muito mais segurança que o nosso software é consistente.

Avaliação preguiçosa tem alguns efeitos assustadores. Vamos dizer que queremos encontrar as _k_ menores elementos em uma lista não ordenada. Em uma linguagem tradicional, a abordagem óbvia seria a de ordenar a lista em primeiro lugar e tirar os _k_  elementos , mas isso é caro. Para maior eficiência, teríamos, ao invés disso escrever uma função especial que encontra esses valores em uma única passagem, e para isso teria que realizar algumas complexas alterações. Em Haskell, a abordagem sort-then-take realmente funciona bem: a avaliação preguiçosa garante que a lista só será classificada o suficiente para encontrar os _k_ elementos mínimos.

Melhor ainda, o nosso código Haskell, que opera de forma tão eficiente é minúsculo e usa funções da biblioteca padrão
```haskell
-- file: ch00/KMinima.hs
-- lines beginning with "--" are comments.

minima k xs = take k (sort xs)
```

Pode demorar um pouco para desenvolver uma percepção intuitiva para quando a avaliação preguiçosa é importante, mas quando nós exploramos-na, o código resultante é limpo, pequeno e eficiente.

Como mostra o exemplo acima, um aspecto importante do poder Haskell reside na compactação do código que escrevemos. Comparado a trabalhar em linguagens populares tradicionais, quando se desenvolver em Haskell, muitas vezes, escrevemos muito menos código, em menos tempo, e substancialmente com menos erros.

### Divertida

Como a programação efetiva em Haskell é muito diferente de outras linguagens, você deve esperar que dominar tanto a linguagem quanto as técnicas de programação funcional exigirá muita dedicação e prática.

Remontando aos nossos dias de começando com Haskell, a boa notícia é que a diversão começa cedo: é simplesmente um desafio agradável a desbravar essa nova linguagem, em que tantas idéias comuns são diferentes ou ausentes, e para descobrir como escrever programas simples.

Para nós, o prazer inicial durou a nossa experiência cresceu e aprofundou nossa compreensão. Em outras linguagens, é difícil ver qualquer conexão entre a ciência e as porcas e parafusos, da programação. Em Haskell, temos algumas idéias importadas da matemática abstrata e colocá-los para funcionar. Mesmo que nós achamos que estas idéias não fáceis de aprender, elas têm um retorno prático para nos ajudar a escrever códigos mais compacto e reutilizável.

Além disso, não vamos estar colocando qualquer pedra em seu caminho: não há particularmente nenhuma técnica difícil neste livro que você deverá dominar a fim de poder programar de forma eficaz.

Dito isto, Haskell é uma linguagem rigorosa: ela vai exigir mais do seu raciocínio daqui para frente. Pode demorar um pouco para conseguir executar o seu primeiro código, com o compilador dizendo que o seu programa não faz sentido. Mesmo com anos de experiência, ficamos surpresos e satisfeitos pela forma como muitas vezes os nossos programas Haskell simplesmente funcionam na primeira tentativa, uma vez que corrigimos esses erros de compilação.

O que esperar deste livro
-------------------------

Começamos este projeto porque um número crescente de pessoas estão usando Haskell para resolver problemas cotidianos. Porque Haskell tem suas raízes na academia, pouco dos livros que existem atualmente sobre Haskell focam sobre problemas e técnicas de programação do dia a dia que estamos interessado.

Com este livro queremos mostrar como usar a programação funcional e Haskell para resolver problemas reais. Este é um livro de referência: cada capítulo contém dezenas de exemplos de código, e muitos contêm aplicações completas. Aqui estão alguns exemplos das bibliotecas, técnicas e ferramentas que nós vamos mostrar-lhe como desenvolver.

*   Criar um aplicativo que baixa os episódios de podcast da internet e armazene-o em um banco de dados SQL
    

*   Testar o seu código de um modo poderosa e intuitiva. Descrever as propriedades que devem ser verdade, então deixe a biblioteca QuickCheck gerar casos de teste automaticamente.
    

*   Pegar um telefone com câmera que captura um código de barras, e transforme-o em um identificador que você pode usar para consulta uma biblioteca ou web site de uma libraria.
    

*   Escrever o código que percorra a web. Troque dados com os servidores e clientes escritos em outras linguagens usando a notação JSON. Desenvolver um verificador da ligação concorrente.
    

### Um pouco sobre você

O que você precisa saber antes de ler este livro? Esperamos que você já saiba programar, mas se você nunca usou uma linguagem funcional, tudo bem.

Não importa o seu nível de experiência, temos tentado antecipar suas necessidades: para explicar novas idéias em profundidade, geralmente usamos exemplos e imagens para conduzir nossas questões.

Como um novo programador Haskell, você vai inevitavelmente começar a escrever alguns códigos a mão onde você poderia ter utilizado uma função de alguma biblioteca ou alguma técnica de programação. Nos empacotamos este livro com informações para ajudá-lo a aprender o mais rapido possível.

Claro, sempre haverá alguns solavancos ao longo da estrada. Se você começar a antecipar uma surpresa ocasional ou dificuldade, juntamente com a diversão, você terá a melhor experiência. Qualquer remendos ásperos você pode bater, não vai durar muito.

Como se tornar um programador mais experiente Haskell, a maneira que você escrever código irá mudar. Com efeito, ao longo deste livro, a forma que apresentamos código irá evoluir, como nos movemos desde o básico da língua até técnicas e recursos poderosos e produtivos.

O que esperar do Haskell
------------------------

Haskell é uma linguagem de programação de propósito geral. Foi concebido sem qualquer nicho de aplicação em mente. Embora ela tenha uma posição firme sobre como os programas devem ser escritos, não favoreça um domínio do problema em detrimento de outros.

Embora em sua essência, a linguagem encoraja um estilo de programação funcional pura e não estrita, esse é o _padrão_, e não a única opção. Haskell também suporta as mais tradicionais modelos de códigos processuais e avaliação rigorosa. Além disso, embora o foco da linguagem está centrado na escrita de programas estaticamente tipados, é possível (embora raramente) escrever código Haskell usando tipos dinâmicos.

### Comparado com as linguagens estáticas tradicionais

Linguagens que somente usam sistemas do tipo estático foram o esteio do mundo da programação durante décadas. Haskell é estaticamente tipados, mas a sua noção do que tipos são, e o que podemos fazer com eles, é muito mais flexível e poderoso do que nas linguagens tradicionais. Tipos dão uma contribuição importante para a concisão, clareza e eficiência dos programas Haskell.

Apesar de poderosa, o sistema de tipos do haskell é muitas vezes também discreto. Se omitir informações de tipos explícito, um compilador Haskell automaticamente inferirá o tipo de uma expressão ou função. Comparado com as linguagens tradicionais estáticas, a que devemos prover grandes quantidades de informações sobre o tipo, a combinação do poder e a inferência do sistema de tipos em Haskell reduz significativamente a desorganização e a redundância do nosso código. <

Vários outros recursos de Haskell se combinam para aumentar a quantidade de trabalho que pode caber em uma tela de texto. Isto traz melhorias no desenvolvimento do tempo e agilidade: podemos criar um código de confiança rapidamente e facilmente refatorar em resposta a novas exigências.

Às vezes, os programas Haskell pode funcionar mais lentamente do que os programas similares escrito em C ou C + +. Para a maioria do código que escrevemos, as grandes vantagens do Haskell de produtividade e confiabilidade superam qualquer desvantagem no desempenho de pequeno porte.

Os processadores multicore estão onipresentes, mas eles permanecem extremamente difícil para o programa com técnicas tradicionais. Haskell oferece tecnologias exclusivas para tornar a programação multicore mais tratáveis. Ele oferece suporte a programação paralela, o software de memória transacional para a simultaneidade de confiança, e escalas de centenas de milhares de threads em simultâneo.

### Comparado com as linguagens dinâmicas modernas

Durante a última década, linguagens interpretada e dinamicamente tipada se tornaram cada vez mais popular. Elas oferecem benefícios substanciais na produtividade do desenvolvedor. Embora estas muitas vezes vem à custa de um enorme impacto na performance, para muitas a produtividade nas tarefas de programação prevalece sobre o desempenho, ou o desempenho não é o fator mais importante de qualquer modo.

Concisão é uma área em que Haskell e linguagens com tipagem dinâmica executam de forma semelhante: em cada caso, escrevemos muito menos código para resolver um problema do que em uma linguagem tradicional. Programas ficam muitas vezes em torno do mesmo tamanho em linguagens dinâmicamente tipadas e Haskell.

Quando se considera o desempenho de execução, Haskell quase sempre tem uma enorme vantagem. Código compilado pelo Glasgow Haskell Compiler (GHC) ficam normalmente entre 20 e 60 vezes mais rápido do que o executado em um interpretador de uma linguagem dinâmicamente tipada. GHC também fornece um intérprete, assim você pode executar scripts sem compilá-los.

Outra grande diferença entre as linguagens dinâmicamente tipadas e Haskell reside na sua filosofia em torno de tipos. Um dos principais motivos para a popularidade de linguagens dinâmicamente tipadas é que raramente precisamos mencionar explicitamente os tipos. Através da inferência de tipo automático, Haskell oferece a mesma vantagem.

Para além desta semelhança superficial, as diferenças são profundas. Em uma linguagem dinamicamente tipada, podemos criar construções que são difíceis de expressar em uma linguagem estaticamente tipada. No entanto, o mesmo acontece em sentido inverso: com um tipo de sistema tão poderosa como o do Haskell, podemos estruturar um programa de uma forma que seria impossível ou inviável em linguagens de tipagem dinâmica.

É importante reconhecer que cada uma dessas abordagens envolve tradeoffs. Muito brevemente, é a escolha entre a perspectiva de segurança que Haskell enfatiza, e a perspectiva de flexibilidade que a tipagem dinâmica  favorece. Se alguém já havia descoberto uma maneira de pensar sobre os tipos de que foi sempre melhor, imaginamos que todos soubessem sobre ela até agora.

Claro, temos nossas próprias opiniões sobre quais vantagens e desvantagens, são mais benéficas. Dois de nós têm anos de experiência em programação em linguagens dinâmicamente tipadas. Nós adoramos trabalhar com elas, nós ainda usamos todos os dias, mas geralmente, nós preferimos Haskell.

### Haskell na indústria e código aberto

Aqui estão apenas alguns exemplos de grandes sistemas de software que tenham sido criados em Haskell. Alguns deles são de código aberto, enquanto outros são produtos patenteados.

*   ASIC e FPGA design software (Lava, produtos da Bluespec Inc.)
    

*   Software de composição musical (Haskore)
    

*   Compiladores e ferramentas relacionadas com compilador (principalmente GHC)
    

*   Controle de distribuição e revisão (Darcs)
    

*   Web middleware (Happstack, produtos de Galois Inc.)
    

*   ABN AMRO é um banco internacional. Ele usa Haskell em investimentos, para medir o risco de contraparte de carteiras de derivativos financeiros.
    

*   Anygma é uma empresa de inicialização. Desenvolve ferramentas de criação multimídia conteúdo usando Haskell.
    

*   Amgen é uma empresa de biotecnologia. Ele cria modelos matemáticos e outras aplicações complexas em Haskell.
    

*   Bluespec é um ASIC e FPGA fornecedor de software de design. Seus produtos são desenvolvidos em Haskell, e as Linguagens de design de chips que fornecem seus produtos são influenciados pelo Haskell.
    

*   Eaton utiliza Haskell para a concepção e verificação de sistemas hidráulicos de veículos híbridos.
    

### Compilação, depuração e análise de desempenho

Para o trabalho prático, quase tão importante como a própria linguagem é o "ecossistema" de bibliotecas e ferramentas à sua volta. Haskell tem uma forte presença nesta área.

O compilador mais utilizado, GHC, foi ativamente desenvolvida por mais de 15 anos, e fornece um conjunto maduro e estável de recursos.

*   Compila para código nativo eficiente em todos os principais sistemas operacionais modernos e arquiteturas de CPU
    

*   Fácil implantação dos binários compilados, livre de restrições de licenciamento
    

*   Análise da cobertura de código
    

*   Detalhado perfis de desempenho e uso de memória
    

*   Documentação completa
    

*   Apoio escalável e massivo para multicore e programação concorrente
    

*   Interpretador e depurador interativo
    

### Bibliotecas bundled e a terceira parte

O compilador GHC vem com uma coleção de bibliotecas úteis. Aqui estão algumas das necessidades comuns de programação que as bibliotecas face a estes.

*   I/O arquivo, e de passagem e manipulação de sistema de arquivos
    

*   Programação de cliente e servidor de rede
    

*   Expressões regulares e análise sintática
    

*   Programação concorrente
    

*   Testes automatizados
    

*   Som e gráficos
    

O banco de dados do pacote Hackage é uma coleção de bibliotecas de código aberto e aplicações da comunidade Haskell. A maioria das bibliotecas publicado em Hackage são licenciados sob os termos que permitem tanto o uso como código aberto e comercial. Algumas das áreas abrangidas pelas bibliotecas de código aberto incluem o seguinte.

*   Interfaces para todas as principais banco de dados open source e comercial.
    

*   Processamento de XML, HTML e XQuery
    

*   Desenvolvimento de cliente e servidor de redes e web
    

*   GUIs Desktop, incluindo toolkits e cross-platform
    

*   Suporte para Unicode e outras codificações de texto
    

Um breve resumo da história do Haskell
--------------------------------------

O desenvolvimento de Haskell está enraizada na matemática e na pesquisa de ciência da computação.

### Pré-História

Algumas décadas antes dos computadores modernos serem inventados, o matemático Alonzo Church desenvolveu uma linguagem chamada de cálculo lambda. Ela destina-se como uma ferramenta para investigar os fundamentos da matemática. A primeira pessoa a perceber a conexão entre práticas de programação e cálculo lambda foi John McCarthy, que criou Lisp em 1958.

Durante os anos 1960, cientistas da computação começaram a reconhecer e estudar a importância do cálculo lambda. Peter Landin e Christopher Strachey desenvolveu idéias sobre os fundamentos das linguagens de programação: sobre como entender o que eles fazem (semântica operacional) e como entender o que eles significam (semântica denotational).

No início dos anos 1970, Robin Milner criou um linguagem de programação funcional mais rigorosa, chamado ML. Embora ML foi desenvolvida para ajudar com provas automatizada de teoremas matemáticos, acabou ganhando uma sequência de outras tarefas de computação em geral.

A década de 1970 viu o surgimento da avaliação preguiçosa (ou função não-estrita) como uma nova estratégia. David Turner desenvolveu SASL e KRC, enquanto Rod Burstall e John Darlington desenvolvido NPL e Hope. NPL, KRC e ML influenciaram o desenvolvimento de várias outras linguagens na década de 1980, incluindo Lazy ML, Clean e Miranda.

### História recente

Até o final dos anos 1980, os esforços dos investigadores que trabalham sobre avaliação preguiçosa em linguagens funcionais estavam espalhados por mais de uma dezena de Linguagens. Preocupados com essa difusão de esforço, um número de investigadores decidiram formar uma comissão para criar uma linguagem comum. Após três anos de trabalho, a comissão publicou a especificação 1.0 do Haskell em 1990. É o nomearam a linguagem depois em homenagem a Haskell Curry, um influente lógico.

Muitas pessoas são realmente suspeitas de falar sobre “design by committee”, mas o trabalho da comissão do Haskell é um belo exemplo dos melhores trabalhos que uma comissão pode fazer. Eles produziram um elegante projeto de linguagem, e conseguiu unificar os esforços divididos da sua comunidade de pesquisa. Do emaranhado de linguagens funcionais com avaliação preguiçosa que existia em 1990, apenas Haskell ainda é ativamente usada

Desde a sua publicação em 1990, o padrão de linguagem Haskell passou por cinco revisões, a mais recente em 1998. Um número de implementações de Haskell foram escritas e várias ainda estão ativamente em desenvolvimento.

Durante a década de 1990, Haskell teve dois objetivos principais. De um lado, ele dava pesquisadores de linguagem uma linguagem estável em que podia experimentar como a avalição preguiçosa podia fazer programas funcionais executar eficientemente. Outros pesquisadores exploraram como construir programas usando técnicas baseados em avaliação preguiçosa funcional. Outros ainda usaram como linguagem de ensino.

### A era moderna

Embora estas explorações básica da década de 1990 prosseguiram, Haskell permaneceu firme como um assunto acadêmico. O slogan informal de quem estava dentro da comunidade era “evitar o sucesso a todo custo”. Poucos fora dessa comunidade tinham ouvido falar da linguagem. Desse modo,  a programação funcional, ficou como um campo bastante obscuro.

Durante este tempo o mainstream mundo da programação experimentava com pequenos ajustes relativamente: a partir de programação em C, até C + +, até Java. Enquanto isso, na periferia, os programadores estavam começando a mexer com o novo, linguagens mais dinâmicas. Guido van Rossum projetou Python, Larry Wall criou Perl e Yukihiro Matsumoto desenvolveu Ruby.

Como essas novas linguagens começaram a infiltrar-se em uma utilização mais ampla, elas espalharam algumas idéias fundamentais. O primeiro foi que os programadores não estavam plenamente trabalhando em linguagens expressivas. A segunda foi, em parte, um subproduto do rápido crescimento no poder computacional da época: muitas vezes é inteligente sacrificar algum desempenho de execução, em troca de um grande aumento na produtividade do programador. Finalmente, várias dessas linguagens emprestaram algo da programação funcional.

Durante a última metade da década passada, Haskell conseguiu escapar da academia, impulsionado em parte pela visibilidade do Python, Ruby e até Javascript. A linguagem agora tem um  rápido e vibrante crescimento na cultura dos usuários open source e comercial, e os pesquisadores continuam a usá-lo para aumentar os limites de desempenho e expressividade.

Recursos úteis
--------------

A medida que você trabalhar com Haskell, com certeza terá dúvidas e irá querer mais informações sobre as coisas. Aqui estão alguns recursos da Internet onde você pode buscar informações e interagir com outros programadores Haskell.

### Material de referência

*   [The Haskell Hierarchical Libraries reference](http://www.haskell.org/ghc/docs/latest/html/libraries/index.html) fornece a documentação para a biblioteca padrão que vem com o compilador. Este é um dos mais valiosos activos online para programadores Haskell.
    

*   Para perguntas sobre a sintaxe e recursos da linguage o [Haskell 98 Report](http://haskell.org/onlinereport/) descreve ao padrão da linguagem Haskell 98.
    

*   Diversas extensões à linguagem tornaram-se comuns desde o Haskell 98 Report foi liberado. O [Guia do usuário do GHC](http://www.haskell.org/ghc/docs/latest/html/users_guide/index.html) contém documentação detalhada sobre as extensões suportadas pelo GHC, bem como algumas características específicas do GHC.
    

*   [Hoogle](http://haskell.org/hoogle/) e [Hayoo](http://holumbus.fh-wedel.de/hayoo/hayoo.html) são os motores de busca da API Haskell. Eles podem procurar as funções pelo nome ou por tipo.
    

### Aplicativos e bibliotecas

Se você está procurando uma biblioteca Haskell a ser usado para uma tarefa específica, ou de um pedido escrito em Haskell, confira os seguintes recursos.

*   A comunidade Haskell mantém um repositório central de bibliotecas de código aberto e aplicações Haskell. É chamado [Hackage](http://hackage.haskell.org/), e permite que você procure software para download, ou procurar a sua coleção por categoria.
    

*   O [Haskell Wiki](http://haskell.org/haskellwiki/Applications_and_libraries) contém uma seção dedicada à informação sobre algumas bibliotecas Haskell.
    

### A comunidade Haskell

Há uma série de maneiras que você pode entrar em contato com outros programadores Haskell, fazer perguntas, saber o que outras pessoas estão falando, e simplesmente fazer alguma rede social com seus pares.

*   A primeira parada em sua busca por recursos da comunidade deve ser o [site Haskell](http://www.haskell.org/). Esta página contém os links mais atuais para várias comunidades e informação, bem como um wiki e mantida ativamente.
    

*   Haskellers utilizar uma série de [listas](http://haskell.org/haskellwiki/Mailing_lists) para os debates de atualidade. Destes, o geralmente mais interessante é chamado `haskell-cafe`. Tem uma atmosfera tranquila e amigável, onde profissionais e acadêmicos trocam informações com hackers casuais e novatos.
    

*   Para bate-papo em tempo real, o [canal de IRC Haskell](http://haskell.org/haskellwiki/IRC_channel), chamado `#haskell`, é grande e animado. Parecido como o `haskell-cafe`, a atmosfera e amigável e prestativos, apesar do grande número de usuários simultâneos.
    

*   Há muitos grupos de usuários locais, meetups, workshops acadêmicos, e assim por diante, aqui está [uma lista dos grupos de usuários conhecidos e workshops](http://haskell.org/haskellwiki/User_groups).
    

*   O [Haskell Weekly News](http://sequence.complete.org/) é um-quase-resumo semanal de muitas atividades na comunidade Haskell. Você pode encontrar links para listas de discussões interessantes, novas versões de softwares, e assim por diante.
    

*   [Haskell Communities and Activities Report](http://haskell.org/communities/) recolhe informações sobre as pessoas que usam Haskell, e o que eles estão fazendo com ele. Ela tem funcionada durante anos, por isso oferece uma boa maneira de descobrir o passado do Haskell.
    

Agradecimentos
--------------

Copyright 2007, 2008 Bryan O'Sullivan, Don Stewart, and John Goerzen. This work is licensed under a [Creative Commons Attribution-Noncommercial 3.0 License](http://creativecommons.org/licenses/by-nc/3.0/). Icons by [Paul Davey](mailto:mattahan@gmail.com) aka [Mattahan](http://mattahan.deviantart.com/).

[Anterior](index.html) [Próximo](getting-started.html)

[Inicio](index.html)
