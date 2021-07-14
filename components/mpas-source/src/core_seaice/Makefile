.SUFFIXES: .F .o
.PHONY: model_forward analysis_members shared

# TY Modified begin: 2021/07/07
.PHONY: sicn_mod
# TY Modified end

all: core_seaice


# TY Modified begin: 2021/07/07
#core_seaice: column_package shared analysis_members model_forward
core_seaice: column_package shared analysis_members model_forward sicn_mod
	ar -ru libdycore.a `find . -type f -name "*.o"`
# TY Modified end

gen_includes:
	$(CPP) $(CPPFLAGS) $(CPPINCLUDES) Registry.xml > Registry_processed.xml
	(if [ ! -d inc ]; then mkdir -p inc; fi) # To generate *.inc files
	(cd inc; $(REG_PARSE) < ../Registry_processed.xml )

core_input_gen:
	if [ ! -e default_inputs ]; then  mkdir default_inputs; fi
	(cd default_inputs; $(NL_GEN) ../Registry_processed.xml namelist.seaice )
	(cd default_inputs; $(ST_GEN) ../Registry_processed.xml streams.seaice stream_list.seaice. listed )

post_build:
	if [ ! -e $(ROOT_DIR)/default_inputs ]; then mkdir $(ROOT_DIR)/default_inputs; fi
	cp default_inputs/* $(ROOT_DIR)/default_inputs/.
	( cd $(ROOT_DIR)/default_inputs; for FILE in `ls -1`; do if [ ! -e ../$$FILE ]; then cp $$FILE ../.; fi; done )

column_package:
	(cd column; $(MAKE))

shared: column_package
	(cd shared; $(MAKE))

analysis_members: column_package shared
	(cd analysis_members; $(MAKE))

model_forward: column_package shared analysis_members
	(cd model_forward; $(MAKE))

# TY Modified begin: 2021/07/07
sicn_mod: 
	(cd sicn_mod; $(MAKE))
# TY Modified end

clean:
	$(RM) *.o *.mod *.f90 libdycore.a
	(cd column; $(MAKE) clean)
	(cd shared; $(MAKE) clean)
	(cd analysis_members; $(MAKE) clean)
	(cd model_forward; $(MAKE) clean)
	$(RM) Registry_processed.xml
	@# Certain systems with intel compilers generate *.i files
	@# This removes them during the clean process
	$(RM) *.i
	$(RM) -r default_inputs
	$(RM) -r inc
