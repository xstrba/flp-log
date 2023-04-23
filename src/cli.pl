%#########################################
% Hamiltonovská kružnice                 #
%                                        #
% @file cli.pl                           #
% @author Boris Štrbák (xstrba05)        #
%                                        #
% Data for testing in cli, always        #
% uncomment only one set of data         #
%#########################################

:- discontiguous nodes/1.
:- discontiguous edge/2.

% Set 1

nodes(['A', 'B', 'C', 'D', 'E', 'F', 'G']).

edge('A','B').
edge('C','A').
edge('B','E').
edge('C','E').
% edge('D','E').
edge('D','F').
edge('F','G').
edge('G','B').

edge('B','A').
edge('A','C').
edge('E','B').
edge('E','C').
% edge('E','D').
edge('F','D').
edge('G','F').
edge('B','G').

% End of set 1

% Set 2

% nodes(['A', 'B', 'C', 'D']).

% edge('A','B').
% edge('A','C').
% edge('A','D').
% edge('A','B').
% edge('B','C').
% edge('B','D').
% edge('C','D').

% edge('B','A').
% edge('C','A').
% edge('D','A').
% edge('B','A').
% edge('C','B').
% edge('D','B').
% edge('D','C').

% End of set 2