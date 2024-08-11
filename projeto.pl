%ist1107294    Rita Cordeiro Dias de Melo
:- set_prolog_flag(answer_write_options,[max_depth(0)]). % para listas completas
:- ['dados.pl'], ['keywords.pl']. % ficheiros a importar

%3.1

%eventosSemSalas/1
/*e verdade se EventosSemSala e uma lista, ordenada e
sem elementos repetidos, de IDs de eventos sem sala*/
eventosSemSalas(EventosSemSala):-
    findall(ID, evento(ID,_,_,_,semSala), EventosSemSala).

%eventosSemSalasDiaSemana/2
/*e verdade se EventosSemSala e uma lista, ordenada e sem elementos repetidos,
de IDs de eventos sem sala que decorrem em DiaDaSemana*/
eventosSemSalasDiaSemana(DiaDaSemana, EventosSemSala):-
    findall(ID, (evento(ID,_,_,_,semSala), horario(ID,DiaDaSemana,_,_,_,_)),
    EventosSemSala).

%Factos Auxiliares
/*auxiliam na conversao do periodo para o semestres correspondente*/
semestresPeriodos(p1, p1_2).  
semestresPeriodos(p2, p1_2).
semestresPeriodos(p3, p3_4).
semestresPeriodos(p4, p3_4).

%eventosSemSalasPeriodo/2
/* e verdade se ListaPeriodos e uma lista de periodos e EventosSemSala
e uma lista de IDs de eventos sem sala nos periodos de ListaPeriodo*/
eventosSemSalasPeriodo([],[]):-!.
eventosSemSalasPeriodo([P|R], EventosSemSala):-
    semestresPeriodos(P,X),
    findall(ID, (evento(ID,_,_,_,semSala), (horario(ID,_,_,_,_,P); horario(ID,_,_,_,_,X))), Lista1),
    eventosSemSalasPeriodo(R, Lista2),
    append(Lista1, Lista2, EventosSemSalaAux),
    sort(EventosSemSalaAux, EventosSemSala).


%3.2   

%organizaEventos/3
/*e verdade se EventosNoPeriodo e a lista, ordenada e sem elementos
repetidos, de IDs dos eventos de ListaEventos que ocorrem no periodo Periodo*/                                    
organizaEventos([],_,[]):-!.
organizaEventos([P|R], Periodo, EventosNoPeriodo):-            
    semestresPeriodos(Periodo,Semestre),
    (horario(P,_,_,_,_,Periodo); horario(P,_,_,_,_,Semestre)),
    organizaEventos(R,Periodo,EventosNoPeriodo2),
    append([P], EventosNoPeriodo2, EventosNoPeriodoAux),
    sort(EventosNoPeriodoAux, EventosNoPeriodo).
organizaEventos([P|R], Periodo, EventosNoPeriodo):-
    semestresPeriodos(Periodo,Semestre),
    horario(P,_,_,_,_,Y),
    (Y \== Periodo, Y \== Semestre),
    organizaEventos(R,Periodo,EventosNoPeriodo).

%eventosMenoresQue/2
/*e verdade se ListaEventosMenoresQue e a lista ordenada e sem elementos repetidos
dos identificadores dos eventos que tem duracao menor ou igual a Duracao*/
eventosMenoresQue(Duracao, ListaEventosMenoresQue):-
    findall(ID, (horario(ID,_,_,_,DuracaoEvento,_), DuracaoEvento =< Duracao), ListaEventosMenoresQue).
    
%eventosMenoresQueBool/2
/*e verdade se o evento identificado por ID tiver
duracao igual ou menor a Duracao*/
eventosMenoresQueBool(ID, Duracao):-
    horario(ID,_,_,_,DuracaoEvento,_),
    DuracaoEvento =< Duracao.

%procuraDisciplinas/2
/*e verdade se ListaDisciplinas e a lista
ordenada alfabeticamente do nome das disciplinas do curso Curso.*/

%procuraDisciplinasAuxiliar/2
/*recebe uma lista de IDs e uma lista de disciplinas*/
procuraDisciplinasAux([],[]):-!.
procuraDisciplinasAux([P|R], ListaDisciplinasAux):-
    findall(NomeDisciplina, evento(P,NomeDisciplina,_,_,_), ListaDisciplinas1),
    procuraDisciplinasAux(R, ListaDisciplinas2),
    append(ListaDisciplinas1,ListaDisciplinas2,ListaDisciplinasAux).
    
procuraDisciplinas(Curso, ListaDisciplinas):-
    findall(ID, turno(ID,Curso,_,_), [P|R]),
    procuraDisciplinasAux([P|R], ListaDisciplinasAux),
    sort(ListaDisciplinasAux, ListaDisciplinas).
        
%organizaDisciplinas/3
/*e verdade se Semestres e uma lista com duas listas. 
A lista na primeira posicao contem as disciplinas de ListaDisciplinas
do curso Curso que ocorrem no primeiro semestre; idem para a lista na
segunda posicao, que contem as que ocorrem no segundo semestre*/

%organizaDisciplinasAux/4
/*recebe uma lista de disciplinas, um curso, uma lista
correspondente as disciplinas do primeiro semestre e uma
lista correspondente as disciplinas do segundo semestre */
organizaDisciplinasAux([],_,[],[]):-!.
organizaDisciplinasAux([L|D], Curso, PrimeiroSemestre, SegundoSemestre):-
    procuraDisciplinas(Curso, DisciplinasCurso),
    member(L, DisciplinasCurso),
    evento(ID,L,_,_,_),
    (horario(ID,_,_,_,_,p1) ; horario(ID,_,_,_,_,p2) ; horario(ID,_,_,_,_,p1_2)),
    organizaDisciplinasAux(D, Curso, PrimeiroSemestre2, SegundoSemestre),
    append([L],PrimeiroSemestre2,PrimeiroSemestre).
organizaDisciplinasAux([L|D], Curso, PrimeiroSemestre, SegundoSemestre):-
    procuraDisciplinas(Curso, DisciplinasCurso),
    member(L, DisciplinasCurso),
    evento(ID,L,_,_,_),
    (horario(ID,_,_,_,_,p3) ; horario(ID,_,_,_,_,p4) ; horario(ID,_,_,_,_,p3_4)),
    organizaDisciplinasAux(D, Curso, PrimeiroSemestre, SegundoSemestre2),
    append([L],SegundoSemestre2,SegundoSemestre).

organizaDisciplinas([L|D], Curso, Semestres):-
    organizaDisciplinasAux([L|D], Curso, PrimeiroSemestre, SegundoSemestre),
    sort(PrimeiroSemestre, PrimeiroSemestreSorted),
    sort(SegundoSemestre, SegundoSemestreSorted),
    append([PrimeiroSemestreSorted],[SegundoSemestreSorted],Semestres).

%horasCurso/4
/*e verdade se TotalHoras for o numero de horas total dos eventos
associadas ao curso Curso, no ano Ano e periodo Periodo*/

%horasCursoAuxiliar/3
/*recebe uma lista de IDs, um perido e o TotalHoras*/
horasCursoAux([],_,0):-!.
horasCursoAux([P|R], Periodo, TotalHoras):-
    semestresPeriodos(Periodo, X),
    (horario(P,_,_,_,Duracao,Periodo) ; horario(P,_,_,_,Duracao,X)),
    horasCursoAux(R, Periodo, TotalHorasAux),
    TotalHoras is TotalHorasAux + Duracao.
horasCursoAux([P|R], Periodo, TotalHoras):-
    semestresPeriodos(Periodo, X),
    horario(P,_,_,_,_,Y),
    (Y \== Periodo , Y \== X),
    horasCursoAux(R, Periodo, TotalHoras).

horasCurso(Periodo, Curso, Ano, TotalHoras):-
    findall(ID, turno(ID, Curso, Ano,_), ListaIDs),
    sort(ListaIDs, ListaIDsSorted),
    horasCursoAux(ListaIDsSorted, Periodo, TotalHoras).

%evolucaoHorasCurso/2
/*e verdade se Evolucao for uma lista de tuplos (Ano, Periodo, NumHoras)
,em que NumHoras e o total de horas do curso Curso, no ano Ano
e periodo Periodo*/

%evolucaoHorasCursoAux2/4
/*recebe um ano, uma lista de peridos, um curso e uma lista
correspondente a evolucao das horas do curso*/
evolucaoHorasCursoAux2(_,[],_,[]):-!.                       %percorre os periodos
evolucaoHorasCursoAux2(A,[P|R],Curso,Evolucao):-
    horasCurso(P,Curso,A,TotalHoras),
    evolucaoHorasCursoAux2(A,R,Curso,Evolucao2Aux),
    append([(A,P,TotalHoras)],Evolucao2Aux,Evolucao).

%evolucaoHorasCursoAux/4
/*recebe uma lista de anos, uma lista de periodos, um curso 
e uma lista correspondente a evolucao das horas do curso*/
evolucaoHorasCursoAux([],_,_,[]):-!.                        %percorre os anos 
evolucaoHorasCursoAux([A|N],[P|R],Curso,Evolucao):-
    evolucaoHorasCursoAux2(A,[P|R],Curso,Evolucao2),
    evolucaoHorasCursoAux(N,[P|R],Curso,Evolucao1),
    append(Evolucao2,Evolucao1,Evolucao).

evolucaoHorasCurso(Curso, EvolucaoSorted):-
    evolucaoHorasCursoAux([1,2,3],[p1,p2,p3,p4],Curso,Evolucao),
    sort(Evolucao, EvolucaoSorted).


%3.3

%ocupaSlot/5
/*e verdade se Horas for o numero de horas sobrepostas entre o evento que
tem inicio em HoraInicioEvento e fim em HoraFimEvento, e o slot
que tem inicio em HoraInicioDada e fim em HoraFimDada.*/              
ocupaSlot(HoraInicioDada, HoraFimDada, HoraInicioEvento, HoraFimEvento, Horas):-
    (HoraFimDada > HoraInicioEvento, HoraFimEvento > HoraInicioDada),
    HoraInicioEvento >= HoraInicioDada,
    HoraFimDada >= HoraFimEvento,
    Horas is HoraFimEvento - HoraInicioEvento.
ocupaSlot(HoraInicioDada, HoraFimDada, HoraInicioEvento, HoraFimEvento, Horas):-
    (HoraFimDada > HoraInicioEvento,HoraFimEvento > HoraInicioDada),
    HoraInicioDada > HoraInicioEvento,
    HoraFimDada >= HoraFimEvento,
    Horas is HoraFimEvento - HoraInicioDada.
ocupaSlot(HoraInicioDada, HoraFimDada, HoraInicioEvento, HoraFimEvento, Horas):-
    (HoraFimDada > HoraInicioEvento,HoraFimEvento > HoraInicioDada),
    HoraInicioEvento >= HoraInicioDada,
    HoraFimEvento > HoraFimDada,
    Horas is HoraFimDada - HoraInicioEvento.
ocupaSlot(HoraInicioDada, HoraFimDada, HoraInicioEvento, HoraFimEvento, Horas):-
    (HoraFimDada > HoraInicioEvento, HoraFimEvento > HoraInicioDada),
    HoraInicioDada > HoraInicioEvento,
    HoraFimEvento > HoraFimDada,
    Horas is HoraFimDada - HoraInicioDada.


%tiposSala/2 (Auxiliar)
/*atraves de um ID, adquire o Tipo de Sala correspondente*/
tiposSala(ID, TipoSala):-
    salas(TipoSala, ListaSalas),
    evento(ID,_,_,_,Sala),
    member(Sala, ListaSalas).

%numHorasOcupadas/6
/*e verdade se SomaHoras for o numero de horas ocupadas nas salas
do tipo TipoSala, no intervalo de tempo entre HoraInicio e HoraFim,
no dia da semana DiaSemana, e no periodo Periodo*/

%numHorasOcupadasAux/4
/*recebe uma lista de IDs, uma HoraInico, uma HoraFim e SomaHoras;
calcula o numero de horas ocupadas*/
numHorasOcupadasAux([],_,_,0):-!.
numHorasOcupadasAux([L|I], HoraInicio, HoraFim, SomaHoras):-
    horario(L,_,HoraInicioEvento,HoraFimEvento,_,_),
    ocupaSlot(HoraInicio, HoraFim, HoraInicioEvento, HoraFimEvento, Horas),
    numHorasOcupadasAux(I, HoraInicio, HoraFim, SomaHoras2),
    SomaHoras is SomaHoras2 + Horas.
numHorasOcupadasAux([L|I], HoraInicio, HoraFim, SomaHoras):-
    horario(L,_,HoraInicioEvento, HoraFimEvento,_,_),
    \+ocupaSlot(HoraInicio, HoraFim, HoraInicioEvento, HoraFimEvento,_),
    numHorasOcupadasAux(I, HoraInicio, HoraFim, SomaHoras).

numHorasOcupadas(Periodo, TipoSala, DiaSemana, HoraInicio, HoraFim, SomaHoras):-
    semestresPeriodos(Periodo,Semestre),
    findall(ID, ((horario(ID, DiaSemana,_,_,_,Periodo);horario(ID, DiaSemana,_,_,_,Semestre)),
    tiposSala(ID,TipoSala)), ListaIDs),
    sort(ListaIDs, ListaIDsSorted),
    numHorasOcupadasAux(ListaIDsSorted, HoraInicio, HoraFim, SomaHoras).


%ocupacaoMax/4
/*e verdade se Max for o numero de horas possiveis de ser 
ocupadas por salas do tipo TipoSala, no intervalo de
tempo entre HoraInicio e HoraFim*/
ocupacaoMax(TipoSala, HoraInicio, HoraFim, Max):-
    salas(TipoSala, ListaSalas),
    length(ListaSalas, SalasEmJogo),                %numero de salas em jogo do tipo TipoSala
    Max is (HoraFim - HoraInicio) * SalasEmJogo.


%percentagem/3
/*e verdade se Percentagem for a divisao de SomaHoras
por Max, multiplicada por 100.*/
percentagem(SomaHoras, Max, Percentagem):-
    Percentagem is SomaHoras / Max * 100.

%ocupacaoCritica/4
/*e verdade se Resultados for uma lista ordenada de tuplos de
casosCriticos(DiaSemana,TipoSala,Percentagem) em que DiaSemana,
TipoSala e Percentagem sao, respectivamente, um dia da semana,
um tipo de sala e a percentagem de ocupacao, no intervalo de
tempo entre HoraInicio e HoraFim, e senso que a percentagem 
de ocupacao esta acima do Threshold*/

%calculaPercentagem/4 (Auxiliar da ocupacao critica)
/*recebe um ID, uma HoraInicio, uma HoraFim, e uma Percentagem;
calcula a percentagem de ocupacao do evento de id ID*/
calculaPercentagem(ID, HoraInicio, HoraFim, Percentagem):-
    horario(ID, DiaSemana,_,_,_,Periodo), 
    tiposSala(ID, TipoSala), 
    numHorasOcupadas(Periodo, TipoSala, DiaSemana, HoraInicio, HoraFim, SomaHoras),
    ocupacaoMax(TipoSala, HoraInicio, HoraFim, Max), 
    percentagem(SomaHoras, Max, Percentagem).


%ocupacaoCriticaAux/4
/*recebe uma lista de ids criticos, uma HoraInicio, uma HoraFim, e uma
lista de resultados que e uma lista de tuplos do tipo
casosCriticos(DiaSemana,TipoSala,Percentagem)*/
ocupacaoCriticaAux([],_,_,[]):-!.
ocupacaoCriticaAux([P|R], HoraInicio, HoraFim, ResultadosAux):-
    tiposSala(P, TipoSala), 
    horario(P, DiaSemana,_,_,_,_),
    calculaPercentagem(P, HoraInicio, HoraFim, Percentagem),
    ceiling(Percentagem, PercentagemFinal),
    ocupacaoCriticaAux(R, HoraInicio, HoraFim, Resultados2),
    append([casosCriticos(DiaSemana, TipoSala, PercentagemFinal)], Resultados2, ResultadosAux).

ocupacaoCritica(HoraInicio, HoraFim, Threshold, Resultados):-
    findall(ID, (calculaPercentagem(ID, HoraInicio, HoraFim, Percentagem),
    Percentagem > Threshold), ListaIDsCriticos),
    sort(ListaIDsCriticos, ListaIDsSorted),
    ocupacaoCriticaAux(ListaIDsSorted, HoraInicio, HoraFim, ResultadosAux),
    sort(ResultadosAux, Resultados).

%3.4

%ocupacaoMesa/3
/*e verdade se ListaPessoas for a lista dos nomes das pessoas a sentar e
ListaRestricoes for a lista de restricoes a verificar e OcupacaoMesa
for uma lista com tres listas, em que a primeira contem as pessoas de um
lado da mesa, a segunda as pessoas a cabeceira e a terceira as pessoas
do outro lado da mesa.*/

cab1(Pessoa, [[_,_,_],[Pessoa,_],[_,_,_]]).
cab2(Pessoa, [[_,_,_],[_,Pessoa],[_,_,_]]).

honra(Pessoa1, Pessoa2, [[_,_,_],[Pessoa1,_],[Pessoa2,_,_]]).    
honra(Pessoa1, Pessoa2, [[_,_,Pessoa2],[_,Pessoa1],[_,_,_]]).                              

lado(Pessoa1, Pessoa2, [[Pessoa1,Pessoa2,_],[_,_],[_,_,_]]).
lado(Pessoa1, Pessoa2, [[Pessoa2,Pessoa1,_],[_,_],[_,_,_]]).
lado(Pessoa1, Pessoa2, [[_,Pessoa1,Pessoa2],[_,_],[_,_,_]]).
lado(Pessoa1, Pessoa2, [[_,Pessoa2,Pessoa1],[_,_],[_,_,_]]).
lado(Pessoa1, Pessoa2, [[_,_,_],[_,_],[Pessoa1,Pessoa2,_]]).
lado(Pessoa1, Pessoa2, [[_,_,_],[_,_],[_,Pessoa2,Pessoa1]]).
lado(Pessoa1, Pessoa2, [[_,_,_],[_,_],[Pessoa2,Pessoa1,_]]).
lado(Pessoa1, Pessoa2, [[_,_,_],[_,_],[_,Pessoa1,Pessoa2]]).

naoLado(Pessoa1, Pessoa2, OcupacaoMesa):- \+lado(Pessoa1, Pessoa2, OcupacaoMesa).

frente(Pessoa1, Pessoa2, [[Pessoa1,_,_],[_,_],[Pessoa2,_,_]]).
frente(Pessoa1, Pessoa2, [[Pessoa2,_,_],[_,_],[Pessoa1,_,_]]).
frente(Pessoa1, Pessoa2, [[_,Pessoa1,_],[_,_],[_,Pessoa2,_]]).
frente(Pessoa1, Pessoa2, [[_,Pessoa2,_],[_,_],[_,Pessoa1,_]]).
frente(Pessoa1, Pessoa2, [[_,_,Pessoa1],[_,_],[_,_,Pessoa2]]).
frente(Pessoa1, Pessoa2, [[_,_,Pessoa2],[_,_],[_,_,Pessoa1]]).

naoFrente(Pessoa1, Pessoa2, OcupacaoMesa):- \+frente(Pessoa1,Pessoa2,OcupacaoMesa).

%aplicaCondicoes/2 (Auxiliar)
/*recebe uma lista de restricoes e uma lista de listas que 
representa a mesa; aplica as restricoes a mesa*/
aplicaRestricoes([],_):-!.                           
aplicaRestricoes([Restricao|Resto], OcupacaoMesa):-
    call(Restricao, OcupacaoMesa),                        %call adiciona o argumento correspondente a ocupacao de mesa
    aplicaRestricoes(Resto, OcupacaoMesa).                %ao predicado correspondente a resticao

ocupacaoMesa(ListaPessoas, ListaRestricoes, OcupacaoMesa):-
    permutation(ListaPessoas, [X1,X2,X3,X4,X5,X6,X7,X8]),         %verificar todas as possibilidades de lugar na mesa
    OcupacaoMesa = [[X1,X2,X3], [X4,X5], [X6,X7,X8]],             %sentar as pessoas na mesa
    aplicaRestricoes(ListaRestricoes, OcupacaoMesa).              %aplicar as condicoes
  

