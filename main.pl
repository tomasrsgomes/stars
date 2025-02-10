% lp24 - ist1114493 - projecto - Tomás Gomes
:- use_module(library(clpfd)). % para poder usar transpose/2
:- set_prolog_flag(answer_write_options,[max_depth(0)]). % ver listas completas
:- [puzzles]. % Ficheiro dado. A avaliação terá mais puzzles.
:- [codigoAuxiliar]. % Ficheiro dado. Não alterar.
% Atenção: nao deves copiar nunca os puzzles para o teu ficheiro de código
% Nao remover nem modificar as linhas anteriores. Obrigado.
% Segue-se o código
%%%%%%%%%%%%



%%% Predicados Principais %%%

% 5.1 Visualização

/**
* visualiza(Lista) é verdade se Lista é uma lista e a aplicação deste predicado permite escrever, 
* por linha, cada elemento da lista Lista.
*
* @param Lista a ser visualizada
*/

visualiza([]).

visualiza([H | T]):-
    writeln(H),
    visualiza(T).    


/**
* visualizaLinha(Lista) é verdade se Lista é uma lista e a aplicação deste predicado permite 
* escrever, por linha, cada elemento da lista Lista, aparecendo antes o número da linha em causa.
*
* @param Lista a ser visualizada
*/
    
visualizaLinha(Lista):-
    visualizaLinhaAux(Lista, 1).


visualizaLinhaAux([], _).

visualizaLinhaAux([Hlista | Tlista], Cont):-
    write(Cont), write(': '), writeln(Hlista),
    Novocont is Cont + 1,
    visualizaLinhaAux(Tlista, Novocont).
    

% 5.2 Inserção de estrelas e pontos

/**
* insereObjecto((L, C), Tabuleiro, Obj) é verdade se Tabuleiro é um tabuleiro que após a aplicação 
* deste predicado passa a ter o objecto Obj nas coordenadas (L,C), caso nestas se encontre uma variável.
*
* @param (L, C) coordenadas onde se pretende inserir o objeto
* @param Tabuleiro onde se pretende inserir o objeto
* @param Obj objeto a ser inserido
*/

insereObjecto((L, C), Tabuleiro, Obj):-
    obtemObjectoTabuleiro((L,C), Tabuleiro, ObjTab),
    var(ObjTab),
    ObjTab = Obj, !.                % O corte impede que se tente unificar o objeto com a variável novamente

insereObjecto(_, _, _).             % Se a coordenada não estiver dentro do tabuleiro, não faz nada


/**
* insereVariosObjectos(ListaCoords, Tabuleiro, ListaObjs) é verdade se ListaCoords for uma lista de
* coordenadas, ListaObjs uma lista de objectos e Tabuleiro um tabuleiro que, após a aplicação do
* predicado, passa a ter nas coordenadas de ListaCoords os objectos de ListaObjs. 
*
* @param ListaCoords lista de coordenadas onde se pretende inserir os objetos
* @param Tabuleiro onde se pretende inserir os objetos
* @param ListaObjs lista de objetos a serem inseridos
*/

insereVariosObjectos([], _, []).

insereVariosObjectos([Coordenada | Tcoords], Tabuleiro, [Objecto | Tobjectos]):-
    length([Coordenada | Tcoords], N),
    length([Objecto | Tobjectos], N),
    insereObjecto(Coordenada, Tabuleiro, Objecto),
    insereVariosObjectos(Tcoords, Tabuleiro, Tobjectos).    


/**
* inserePontosVolta(Tabuleiro, (L, C)) é verdade se Tabuleiro é um tabuleiro que, após a aplicação 
* do predicado, passa a ter pontos (p) à volta das coordenadas (L, C).
* 
* @param Tabuleiro onde se pretende inserir os pontos
* @param (L, C) coordenadas à volta da qual se pretende inserir os pontos
*/

inserePontosVolta(Tabuleiro, (L, C)):-
    Lcima is L - 1,
    Lbaixo is L + 1,
    Cesq is C - 1,
    Cdir is C + 1,                                                                 
    insereVariosObjectos(
        [(Lcima, Cesq), (Lcima, C), (Lcima, Cdir), (L, Cesq), (L, Cdir), 
         (Lbaixo, Cesq), (Lbaixo, C), (Lbaixo, Cdir)], 
        Tabuleiro,
        [p, p, p, p, p, p, p, p]
    ).    


/**
* inserePontos(Tabuleiro, ListaCoord) é verdade se Tabuleiro é um tabuleiro que, após a aplicação do
* predicado, passa a ter pontos (p) em todas as coordenadas de ListaCoord.
*
* @param Tabuleiro onde se pretende inserir os pontos
* @param ListaCoord lista de coordenadas onde se pretende inserir os pontos
*/

inserePontos(Tabuleiro, ListaCoord):-
    length(ListaCoord, N),
    length(ListaObjs, N),               % Cria uma lista de tamanho N
    maplist(=(p), ListaObjs),               % Unifica todos os elementos da lista com pontos (p)
    insereVariosObjectos(ListaCoord, Tabuleiro, ListaObjs).    


% 5.3 Consultas

/**
* objectosEmCoordenadas(ListaCoords, Tabuleiro, ListaObjs) é verdade se ListaObjs for a lista de
* objectos (pontos, estrelas ou variáveis) das coordenadas ListaCoords no tabuleiro Tabuleiro,
* apresentados na mesma ordem das coordenadas.
*
* @param ListaCoords lista de coordenadas onde se encontram os objectos
* @param Tabuleiro tabuleiro onde se encontram os objectos
* @param ListaObjs lista de objectos nessas coordenadas
*/

objectosEmCoordenadas([], _, []).

objectosEmCoordenadas([(L,C) | Tcoords], Tabuleiro, [Obj | ListaObjs]):-
    coorDentroTabuleiro((L,C), Tabuleiro),
    obtemObjectoTabuleiro((L,C), Tabuleiro, Obj),
    objectosEmCoordenadas(Tcoords, Tabuleiro, ListaObjs).    


/**
* coordObjectos(Objecto, Tabuleiro, ListaCoords, ListaCoordObjs, NumObjectos) é verdade se Tabuleiro
* for um tabuleiro, Listacoords uma lista de coordenadas e ListaCoordObjs a sublista de ListaCoords
* que contém as coordenadas dos objectos do tipo Objecto, tal como ocorrem no tabuleiro. NumObjectos
* é o número de objectos Objecto encontrados. ListaCoordObjs deve estar ordenada por linha e coluna.
*
* @param Objecto objecto a ser procurado
* @param Tabuleiro tabuleiro onde se pretende procurar o objecto
* @param ListaCoords lista de coordenadas onde se pretende procurar o objecto
* @param ListaCoordObjs lista de coordenadas onde se encontram os objectos
* @param NumObjectos número de objectos encontrados
*/ 

coordObjectos(Objecto, Tabuleiro, ListaCoords, SortedListaCoordObjs, NumObjectos):-
    coordObjectosAux(Objecto, Tabuleiro, ListaCoords, ListaCoordObjs, NumObjectos, 0),              % Será devolvida uma lista de coordenadas desordenada
    sort(ListaCoordObjs, SortedListaCoordObjs).             % Ordena essa lista


coordObjectosAux(_, _, [], [], NumObjectos, NumObjectos):- !.

coordObjectosAux(Objecto, Tabuleiro, [(L,C) | Tcoords], [(L,C) | ListaCoordObjs], NumObjectos, AuxNObj):-
    obtemObjectoTabuleiro((L,C), Tabuleiro, Obj),
    (var(Objecto) -> var(Obj); Obj == Objecto), !,              % O corte impede uma nova tentativa de unificação de um objeto correspondente ao pretendido 
    NovoAuxNObj is AuxNObj + 1,
    coordObjectosAux(Objecto, Tabuleiro, Tcoords, ListaCoordObjs, NumObjectos, NovoAuxNObj).    

coordObjectosAux(Objecto, Tabuleiro, [_ | Tcoords], ListaCoordObjs, NumObjectos, AuxNObj):-
    !,              % O corte impede uma nova tentativa de unificação após ignorar uma coordenada com um objeto não correspondente ao pretendido
    coordObjectosAux(Objecto, Tabuleiro, Tcoords, ListaCoordObjs, NumObjectos, AuxNObj).    


/**
* coordenadasVars(Tabuleiro, ListaVars) é verdade se ListaVars forem as coordenadas das variáveis do
* tabuleiro Tabuleiro. Mais uma vez, ListaVars deve estar ordenada por linhas e colunas.
*
* @param Tabuleiro tabuleiro onde se pretende procurar as variáveis
* @param ListaVars lista de coordenadas onde se encontram as variáveis
*/

coordenadasVars(Tabuleiro, ListaVars):-
    coordenadasVarsAux(Tabuleiro, ListaVars, 1).


coordenadasVarsAux([], [], _).

coordenadasVarsAux([Linha | TTabuleiro], ListaVarsCompleta, Nlinha):-
    coordenadasVarsLinha(Linha, ListaVarsLinha, Nlinha, 1),             % Procura as variáveis numa linha
    ProxLinha is Nlinha + 1,
    coordenadasVarsAux(TTabuleiro, ListaVars, ProxLinha),               % Procura as variáveis na próxima linha
    append(ListaVarsLinha, ListaVars, ListaVarsCompleta).


coordenadasVarsLinha([], [], _, _).

coordenadasVarsLinha([Obj | TLinha], [(Nlinha, Ncoluna) | ListaVars], Nlinha, Ncoluna):-
    var(Obj), !,                % O cortes impede que se tente unificar a variável com a coordenada novamente
    ProxColuna is Ncoluna + 1,
    coordenadasVarsLinha(TLinha, ListaVars, Nlinha, ProxColuna).

coordenadasVarsLinha([_ | TLinha], ListaVars, Nlinha, Ncoluna):-
    ProxColuna is Ncoluna + 1,
    coordenadasVarsLinha(TLinha, ListaVars, Nlinha, ProxColuna).


% 5.4 Estratégias
% 5.4.1 Fechar linhas, colunas ou estruturas

/**
* fechaListaCoordenadas(Tabuleiro, ListaCoord) que é verdade se Tabuleiro for um tabuleiro e 
* ListaCoord for uma lista de coordenadas; após a aplicação deste predicado, as coordenadas de
* ListaCoord deverão ser apenas estrelas e pontos, considerando as hipóteses definidas anteriormente
* (h1-h2-h3).
*
* @param Tabuleiro tabuleiro onde se pretende fechar as coordenadas
* @param ListaCoord lista de coordenadas a serem fechadas
*/

% H1 - Se houver duas estrelas, as variáveis são pontos
fechaListaCoordenadas(Tabuleiro, ListaCoord):-
    coordObjectos(e, Tabuleiro, ListaCoord, _, Nestrelas),
    Nestrelas = 2, !,               % O corte impede que se tente fechar a lista de coordenadas novamente
    inserePontos(Tabuleiro, ListaCoord).

% H2 - Se houver uma estrela e uma variável, a variável é uma estrela
fechaListaCoordenadas(Tabuleiro, ListaCoord):-
    coordObjectos(e, Tabuleiro, ListaCoord, _, Nestrelas),
    Nestrelas = 1,
    coordObjectos(_, Tabuleiro, ListaCoord, [(L,C)], Nvariaveis),
    Nvariaveis = 1, !,              % O corte impede que se tente fechar a lista de coordenadas novamente
    insereObjecto((L,C), Tabuleiro, e),
    inserePontosVolta(Tabuleiro, (L,C)).

% H3 - Se não houver estrelas e houver duas variáveis, estas são estrelas
fechaListaCoordenadas(Tabuleiro, ListaCoord):-
    coordObjectos(e, Tabuleiro, ListaCoord, _, Nestrelas),
    Nestrelas = 0,
    coordObjectos(_, Tabuleiro, ListaCoord, [(L1,C1), (L2,C2)], Nvariaveis),
    Nvariaveis = 2, !,              % O corte impede que se tente fechar a lista de coordenadas novamente
    insereVariosObjectos([(L1,C1), (L2,C2)], Tabuleiro, [e,e]),
    inserePontosVolta(Tabuleiro, (L1,C1)),
    inserePontosVolta(Tabuleiro, (L2,C2)).

fechaListaCoordenadas(_, _).                % Se não se verificar nenhuma das condições anteriores, não faz nada


/**
* fecha(Tabuleiro, ListaListasCoord) que é verdade se Tabuleiro for um tabuleiro e ListaListasCoord
* for uma lista de listas de coordenadas. Após a aplicação deste predicado, Tabuleiro será o
* resultado de aplicar o predicado anterior a cada lista de coordenadas.
*
* @param Tabuleiro tabuleiro onde se pretende fechar as coordenadas
* @param ListaListasCoord lista de listas de coordenadas a serem fechadas
*/

fecha(_, []):- !.

fecha(Tabuleiro, [ListaCoords | RestoListasCoords]):-
    fechaListaCoordenadas(Tabuleiro, ListaCoords),
    fecha(Tabuleiro, RestoListasCoords).


% 5.4.2 Encontrar padrões

/**
* encontraSequencia(Tabuleiro, N, ListaCoords, Seq) que é verdade se Tabuleiro for um tabuleiro,
* ListaCoords for uma lista de coordenadas e N o tamanho de Seq, que é uma sublista de ListaCoords.
*
* @param Tabuleiro tabuleiro onde se pretende procurar a sequência
* @param N tamanho da sequência
* @param ListaCoords lista de coordenadas onde se pretende procurar a sequência
* @param Seq sublista de ListaCoords
*/

encontraSequencia(Tabuleiro, N, ListaCoords, Seq):-
    coordObjectos(e, Tabuleiro, ListaCoords, _, 0),
    encontraSequenciaAux(Tabuleiro, N, ListaCoords, Seq, 0),
    ehSublista(Seq, ListaCoords), !.                % O corte impede que se verifique a sequência mais que uma vez


encontraSequenciaAux(_, N, [], [], N).              % Se a lista de coordenadas estiver vazia e O número de Variáveis for N, termina a recursão - Caso base

encontraSequenciaAux(Tabuleiro, N, [(L,C) | TListaCoords], [(L,C) | Seq], AuxNVars):-
    obtemObjectoTabuleiro((L,C), Tabuleiro, Obj),
    var(Obj), !,                % O corte impede que se tente unificar a variável com a coordenada novamente
    NovoAuxNVars is AuxNVars + 1,
    encontraSequenciaAux(Tabuleiro, N, TListaCoords, Seq, NovoAuxNVars).

encontraSequenciaAux(Tabuleiro, N, [_ | TListaCoords], Seq, AuxNVars):-
    encontraSequenciaAux(Tabuleiro, N, TListaCoords, Seq, AuxNVars).


/**
* aplicaPadraoI(Tabuleiro, [(L1, C1), (L2, C2), (L3, C3)]), que é verdade se Tabuleiro for um
* tabuleiro e [(L1, C1), (L2, C2), (L3, C3)] for uma lista de coordenadas. Após a aplicação deste
* predicado, Tabuleiro será o resultado de colocar uma estrela (e) em (L1, C1) e (L3, C3) e os
* obrigatórios pontos (p) à volta de cada estrela.
*
* @param Tabuleiro tabuleiro onde se pretende aplicar o padrão
* @param [(L1, C1), (L2, C2), (L3, C3)] lista de coordenadas onde se pretende aplicar o padrão
*/

aplicaPadraoI(Tabuleiro,  [(L1, C1), _, (L3, C3)]):-
    insereVariosObjectos([(L1, C1), (L3, C3)], Tabuleiro, [e,e]),
    inserePontosVolta(Tabuleiro, (L1, C1)),
    inserePontosVolta(Tabuleiro, (L3, C3)).


/**
* aplicaPadroes(Tabuleiro, ListaListaCoords) que é verdade se Tabuleiro for um tabuleiro,
* ListaListaCoords for uma lista de listas com coordenadas; após a aplicação deste predicado
* ter-se-ão encontrado sequências de tamanho 3 e aplicado o aplicaPadraoI/2, ou então ter-se-ão
* encontrado sequências de tamanho 4 e aplicado o aplicaPadraoT/2.
*
* @param Tabuleiro tabuleiro onde se pretende aplicar os padrões
* @param ListaListaCoords lista de listas de coordenadas onde se pretende aplicar os padrões
*/

aplicaPadroes(_,[]).

aplicaPadroes(Tabuleiro, [ListaCoords | TListaListasCoords]):-
    encontraSequencia(Tabuleiro, 4, ListaCoords, Seq),
    aplicaPadraoT(Tabuleiro, Seq),              % Se encontrar uma sequência de 4, aplica o padrão T
    aplicaPadroes(Tabuleiro, TListaListasCoords), !.                % O corte impede que se tente aplicar o padrão mais que uma vez

aplicaPadroes(Tabuleiro, [ListaCoords | TListaListasCoords]):-
    encontraSequencia(Tabuleiro, 3, ListaCoords, Seq),
    aplicaPadraoI(Tabuleiro, Seq),              % Se encontrar uma sequência de 3, aplica o padrão I
    aplicaPadroes(Tabuleiro, TListaListasCoords), !.                    % O corte impede que se tente aplicar o padrão mais que uma vez

aplicaPadroes(Tabuleiro, [_ | TListaListasCoords]):-
    aplicaPadroes(Tabuleiro, TListaListasCoords).


%% 5.5 Apoteose Final

/**
* resolve(Estruturas, Tabuleiro) é verdade se Estrutura for uma estrutura e Tabuleiro for um
* tabuleiro que resulta de aplicar os predicados aplicaPadroes/2 e fecha/2 até já não haver mais
* alterações nas variáveis do tabuleiro.
*
* @param Estruturas estruturas do tabuleiro
* @param Tabuleiro tabuleiro a ser resolvido
*/

resolve(Estruturas, Tabuleiro):-
    coordTodas(Estruturas, CoordTodas),
    copy_term(Tabuleiro, CopiaTabuleiro),               % Copia o tabuleiro para poder comparar com o tabuleiro inicial com o que resulta da aplicação dos padrões e do fecha
    resolveAux(Tabuleiro, CoordTodas, CopiaTabuleiro).


resolveAux(Tabuleiro, CoordTodas, TabuleiroAnterior):-
    aplicaPadroes(Tabuleiro, CoordTodas),
    fecha(Tabuleiro, CoordTodas),
    (Tabuleiro =@= TabuleiroAnterior ->             % Compara os tabuleiros para ver se houve alterações
        true  % Termina recursão quando o tabuleiro não muda
    ;
        copy_term(Tabuleiro, NovoTabuleiro),                % Copia o tabuleiro para poder comparar com o tabuleiro inicial com o que resulta da aplicação dos padrões e do fecha
        resolveAux(Tabuleiro, CoordTodas, NovoTabuleiro)
    ), !.               % O corte impede que se tente resolver o tabuleiro mais que uma vez



%%% Predicados Auxiliares %%%

/**
* obtemObjectoTabuleiro((L,C), Tabuleiro, Obj) é verdade se Tabuleiro é um tabuleiro e Obj é o
* objecto que se encontra nas coordenadas (L,C) do tabuleiro.
*
* @param (L,C) coordenadas do objecto
* @param Tabuleiro tabuleiro onde se encontra o objecto
* @param Obj objecto a ser retornado
*/

obtemObjectoTabuleiro((L,C), Tabuleiro, Obj):-
    nth1(L, Tabuleiro, Linha),
    nth1(C, Linha, Obj).    


/**
* coordDentreTabuleiro((L, C), Tabuleiro) é verdade se (L, C) é uma coordenada válida dentro do
* tabuleiro Tabuleiro.
*
* @param (L, C) coordenada a serem verificada
* @param Tabuleiro tabuleiro onde se pretende verificar a coordenada
*/

coorDentroTabuleiro((L, C), Tabuleiro):-
    length(Tabuleiro, N),
    between(1, N, L),
    between(1, N, C). 
    

/**
* ehSublista(Sublista, Lista) é verdade se Sublista é uma sublista de Lista.
*
* @param Sublista sublista a ser verificada
* @param Lista lista onde se pretende verificar a sublista
*/

ehSublista(Sublista, Lista) :-
    append(_, ListaAux, Lista),
    append(Sublista, _, ListaAux).