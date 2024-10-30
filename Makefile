FC = ifx
TARGET = test
SRCS = global.f90 geometry.f90 utility.f90 data_structures.f90 geometric_data.f90 unstructured_grid.f90 tetrahedral.F90 test.f90
OBJS = $(SRCS:.f90=.o)
OBJS := $(OBJS:.F90=.o)
MOD_FILES = $(SRCS:.f90=.mod)
MOD_FILES := $(MOD_FILES:.F90=.mod)

# Base flags
FFLAGS = -fpp -qopenmp

# Run clean before building to remove .o and .mod files
all: clean $(TARGET)

# Build the target from object files
$(TARGET): $(OBJS)
	$(FC) $(FFLAGS) -o $@ $(OBJS)
	@echo $(OMP_STATUS)
	@rm -f $(OBJS) $(MOD_FILES)  # Clean up .o and .mod files after compilation

# Compile .f90 and .F90 files into .o
%.o: %.f90
	$(FC) $(FFLAGS) -c $< -o $@

%.o: %.F90
	$(FC) $(FFLAGS) -c $< -o $@

# Clean rule to remove all .o, .mod, and the executable
clean:
	rm -f $(OBJS) $(MOD_FILES) $(TARGET)

.PHONY: all clean
