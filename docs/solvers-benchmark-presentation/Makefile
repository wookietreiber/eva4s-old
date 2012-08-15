
  # -----------------------------------------------------------------------
  # names, paths
  # -----------------------------------------------------------------------

NAME = eva4s

TARGET_BASE_DIR = /tmp/typesetting/$(NAME)

  # -----------------------------------------------------------------------
  # programs, flags, environment variables
  # -----------------------------------------------------------------------

CP_F    = cp -f
RM_F    = rm -f
MKDIR_P = mkdir -p

TEXMFOUTPUT := $(TARGET_BASE_DIR)
TEXINPUTS   := ".:$(TARGET_BASE_DIR)"

LATEX_FLAGS = -output-directory $(TARGET_BASE_DIR)

PDF_LATEX = pdflatex -jobname $(NAME) $(LATEX_FLAGS) $(NAME).tex

  # -----------------------------------------------------------------------
  # targets
  # -----------------------------------------------------------------------

default: all

all: main

target-dirs:
	$(MKDIR_P) $(TARGET_BASE_DIR)

main: target-dirs
	$(PDF_LATEX)    && \
	$(PDF_LATEX)

