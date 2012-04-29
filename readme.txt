################################################################################
#                                                                              #
#  Predmet: Distribuovane programove systemy                                   #
#  Tema:    Distribuovane vyhladavanie                                         #
#                                                                              #
#  Bc. Milan Laslop, Bc. Tomas Hurban                                          #
#  2012, April                                                                 #
#                                                                              #
################################################################################

Index:
1. Obsah adresarov
2. Skompilovanie projektu
3. Spustenie projektu
4. Testovanie

################################################################################
# 1. Obsah adresarov
################################################################################

doc - obsahuje dokumentaciu a prezentaciu k projektu
impl/dsearch - adresar s projektom pre Eclipse + Erlang
impl/dsearch/src - obsahuje zdrojove kody projektu

################################################################################
# 2. Skompilovanie projektu
################################################################################

Projekt bol vytvoreny v prostredi Eclipse + Erlang, staci ho importovat:

File->Import->Existing Projects into Workspace

################################################################################
# 3. Spustenie projektu
################################################################################

1. Spustenie centralneho servera:

	central_server:start_link()

2. Nahratie obsahu na server:

	central_server:update(PartName, PartData)

3. Pripojenie jedneho alebo viacerych poskytovatelov vyhladavanie:

	search_provider_supervisor:start_link()

4. Vyhladavanie v obsahu:
	
	{ok, Results} = central_server:search(SearchTerm)

################################################################################
# 4. Testovanie
################################################################################

Ukazky testovacich funkcii je mozne najst v module tests. 
Najskor je potrebne spustit server pomocou funkcie start() a potom je mozne skusat testovacie funkcie.

System jednotlive cinnosti zapisuje do log suboru (log.txt), ktory sa nachadza vo workspace adresari Eclipse.