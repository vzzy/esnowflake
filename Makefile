REBAR = rebar3
COOKIE = esnowflake

git:
	git pull
	git add -A
	git commit -m '新增方法'	
	git push origin master

clean:
	rm -rf _build
	rm -rf rebar.lock
	rm -rf erl_crash.dump
	
all: 
	$(REBAR) clean
	$(REBAR) compile
	erl -pa _build/default/lib/*/ebin -pa _build/default/lib/*/priv -name '$(COOKIE)@127.0.0.1' -setcookie $(COOKIE) -s $(COOKIE)
	
.PHONY:all clean git


