#*****************************************************************************#
#                                                                             #
#   Vincent Jacquier                                     :::      ::::::::    #
#   Jean-Jacques MOIROUX                               :+:      :+:    :+:    #
#                                                    +:+ +:+         +:+      #
#   By: vjacquie <vjacquie@student.42.fr>          +#+  +:+       +#+         #
#   By: jmoiroux <jjmoiroux@gmail.com>           +#+#+#+#+#+   +#+            #
#                                                     #+#    #+#              #
#   Created: 2015/18/15 by vjacquie jmoiroux         ###   ########.fr        #
#                                                                             #
#*****************************************************************************#

NAME = tic-tac-toc

SOURCES = Case.ml DataSet.ml Map.ml main.ml

CAMLC = ocamlc
CAMLOPT = ocamlopt
CAMLDEP = ocamldep
# deprecated
FLAGS = -w da

LIBS = 
WITHGRAPHICS = graphics.cma -cclib -LGraphics

all: depend $(NAME).byt

$(NAME): opt byt
	ln -s $(NAME).byt $(NAME)

opt: $(NAME).opt
byt: $(NAME).byt

OBJS = $(SOURCES:.ml=.cmo)
OPTOBJS = $(SOURCES:.ml=.cmx)

$(NAME).byt: $(OBJS)
	$(CAMLC) $(FLAGS) -o $(NAME).byt $(LIBS) $(OBJS)

$(NAME).opt: $(OPTOBJS)
	$(CAMLOPT) $(FLAGS) -o $(NAME).opt $(LIBS:.cma=.cmxa) $(OPTOBJS)

.SUFFIXES:
.SUFFIXES: .ml .mli .cmo .cmi .cmx

.ml.cmo:
	$(CAMLC) $(FLAGS) -c $<

.mli.cmi:
	$(CAMLC) $(FLAGS) -c $<

.ml.cmx:
	$(CAMLOPT) $(FLAGS) -c $<

clean:
	rm -f *.cm[iox] *~ .*~ *.o
	rm -f $(NAME).o

fclean: clean
	rm -f $(NAME)
	rm -f $(NAME).opt
	rm -f $(NAME).byt

depend: .depend
	$(CAMLDEP) $(SOURCES) > .depend

re: fclean all

include .depend
