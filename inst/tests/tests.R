


f1 <- form(action="http://foo.com", method="POST", type="x-www-form")
ti <- text_input(name='name', default='Pete')
si <- select_input(name='agegroup', options=c(a="< 18", b="over 18"))

add_element(f1, ti)
add_element(f1, si)
print(f1)
cat(f1)