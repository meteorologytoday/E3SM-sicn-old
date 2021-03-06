ifeq "$(ROOT_DIR)" ""
   ROOT_DIR=$(shell pwd)/src
endif
EXE_NAME=seaice_model
NAMELIST_SUFFIX=seaice
FCINCLUDES += -I$(ROOT_DIR)/core_seaice/column -I$(ROOT_DIR)/core_seaice/shared -I$(ROOT_DIR)/core_seaice/analysis_members -I$(ROOT_DIR)/core_seaice/model_forward 
# TY modified begin: 2021/07/07
FCINCLUDES += -I$(ROOT_DIR)/core_seaice/sicn_mod
# TY modified end 
override CPPFLAGS += -DCORE_SEAICE
ifneq "$(ESM)" ""
override CPPFLAGS += -Dcoupled -DCCSMCOUPLED
endif

report_builds:
	@echo "CORE=seaice"
