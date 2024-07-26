GHC = ghc
GHCFLAGS = -Wall -O2

SRCDIR = src
SRC0 = $(SRCDIR)/Types.hs $(SRCDIR)/Lexer.hs $(SRCDIR)/Parser.hs
SRC1 = $(SRCDIR)/Translator1.hs $(SRCDIR)/Main.hs
SRC2 = $(SRCDIR)/Translator2.hs $(SRCDIR)/Main.hs
SRC3 = $(SRCDIR)/Translator3.hs $(SRCDIR)/Main.hs
SRC4 = $(SRCDIR)/Translator4.hs $(SRCDIR)/Main.hs
SRC5 = $(SRCDIR)/Stats.hs $(SRCDIR)/MainStats.hs

BLDDIR = build
TARGET1 = $(BLDDIR)/lpod2asp1
TARGET2 = $(BLDDIR)/lpod2asp2
TARGET3 = $(BLDDIR)/lpod2asp3
TARGET4 = $(BLDDIR)/lpod2asp4
TARGET5 = $(BLDDIR)/lpodstats

.PHONY: all clean

all: $(TARGET1) $(TARGET2) $(TARGET3) $(TARGET4) $(TARGET5)

$(TARGET1): $(SRC0) $(SRC1)
	$(GHC) $(GHCFLAGS) -o $(TARGET1) $(SRC0) $(SRC1)

$(TARGET2): $(SRC0) $(SRC2)
	$(GHC) $(GHCFLAGS) -o $(TARGET2) $(SRC0) $(SRC2)

$(TARGET3): $(SRC0) $(SRC3)
	$(GHC) $(GHCFLAGS) -o $(TARGET3) $(SRC0) $(SRC3)

$(TARGET4): $(SRC0) $(SRC4)
	$(GHC) $(GHCFLAGS) -o $(TARGET4) $(SRC0) $(SRC4)

$(TARGET5): $(SRC0) $(SRC5)
	$(GHC) $(GHCFLAGS) -o $(TARGET5) $(SRC0) $(SRC5)

$(SRCDIR)/Lexer.hs: $(SRCDIR)/Lexer.x
	alex -o $(SRCDIR)/Lexer.hs $(SRCDIR)/Lexer.x

$(SRCDIR)/Parser.hs: $(SRCDIR)/Parser.y
	happy -o $(SRCDIR)/Parser.hs $(SRCDIR)/Parser.y

clean:
	rm -f $(BLDDIR)/* $(SRCDIR)/*.o $(SRCDIR)/*.hi $(SRCDIR)/Lexer.hs $(SRCDIR)/Parser.hs
