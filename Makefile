SRC = "./src/"
OUT = "./out/"

all:
	ghc --make -i$(SRC) -o s2hk $(SRC)Main.hs -outputdir $(OUT)

uuagc:
	uuagc --data --catas --semfuns --signatures $(SRC)Estructura.ag -P $(SRC)
