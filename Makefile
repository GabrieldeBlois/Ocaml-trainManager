NAME 				=		trainmanager

ML					=		train.ml \
								date.ml \
								link.ml \
								station.ml \
								trip.ml \
								trainManager.ml \
								cmd.ml

MLI 				=  	train.mli \
								date.mli \
								link.mli \
								station.mli \
								trip.mli \
								trainManager.mli

CMI 				= 	$(MLI:.mli=.cmi)
CMO 				= 	$(ML:.ml=.cmo)
CMX 				= 	$(ML:.ml=.cmx)


OCAMLDPE 		= 	ocamldep
CAMLFLAGS 	= 	-w Aelz -warn-error A
OCAMLC 			= 	ocamlc str.cma $(CAMLFLAGS)
OCAMLOPT 		= 	ocamlopt str.cmxa unix.cmxa $(CAMLFLAGS)
OCAMLDOC 		= 	ocamldoc -html -d $(ROOT)/doc


all:		.depend $(CMI) $(NAME)

byte:		.depend $(CMI) $(NAME).byte


$(NAME):	$(CMX)
		@$(OCAMLOPT) -o $@ $(CMX)
		@echo "[OK] $(NAME) linked"

$(NAME).byte:	$(CMO)
		@$(OCAMLC) -o $@ $(CMO)
		@echo "[OK] $(NAME).byte linked"

%.cmx:		%.ml
		@$(OCAMLOPT) -c $<
		@echo "[OK] [$<] builded"

%.cmo:		%.ml
		@$(OCAMLC) -c $<
		@echo "[OK] [$<] builded"

%.cmi:		%.mli
		@$(OCAMLC) -c $<
		@echo "[OK] [$<] builded"

documentation:  $(CMI)
		@$(OCAMLDOC) $(MLI)
		@echo "[OK] Documentation"


re:		fclean all

clean:
		@/bin/rm -f *.cm* *.o .depend *~
		@echo "[OK] clean"


fclean: 	clean
		@/bin/rm -f $(NAME) $(NAME).byte
		@echo "[OK] fclean"


.depend:
		@/bin/rm -f .depend
		@$(OCAMLDPE) $(MLI) $(ML) > .depend
		@echo "[OK] dependencies"
