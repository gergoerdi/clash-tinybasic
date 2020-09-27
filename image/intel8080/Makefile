all: alpha-basic1000.a80.com

%.com: %.hex
	srec_cat $< -intel -o $@ -binary

