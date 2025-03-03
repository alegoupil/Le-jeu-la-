Caml1999M028����            /lib/quadtree.ml����  v�  �  `l  _�����1ocaml.ppx.context��&_none_@@ �A����������)tool_name���*ppx_driver@@@����,include_dirs����"[]@@@����)load_path!����
%@%@@����,open_modules*����.@.@@����+for_package3����$None8@8@@����%debug=����%falseB@B@@����+use_threadsG����
K@K@@����-use_vmthreadsP����T@T@@����/recursive_typesY����]@]@@����)principalb����%f@f@@����3transparent_modulesk����.o@o@@����-unboxed_typest����7x@x@@����-unsafe_string}����@�@�@@����'cookies�����"::�����������,inline_tests�@�@@����'enabled��.<command-line>A@A�A@H@@��A@@�A@I@@@@�@@����������������,library-name�@�@@����,libnewtonoid��A@A�A@M@@��A@N@@@@�@@�������@�@@@�@@�@@@�@@�@@@@�@@@�@�����@������"()��/lib/quadtree.mlBggA@@@��������3Ppx_inline_test_lib'Runtime5set_lib_and_partition@@��@���(@@@��@��� @@@@@@@@���A�    �%coord�� Bgl�!Bgq@@@@A��������%float��,Bgt�-Bgy@@�@@@�����%float��6Bg|�7Bg A@@�@@@@�@@@@�;@@�<@���A�    �%bound��DE � ��EE � �@@@@A��������%coord��PE � ��QE � �@@�@@@�����%coord��ZE � ��[E � �@@�@@@@�@@@@��_E � �@@�@���@�����2center_from_bounds��kL���lL��@�@@@��@@���������"x1��{L���|L��@�@@@����"y1���L����L��@�@@@@���L����L��@��@@@�������"x2���L����L��@�@@@����"y2���L����L��@�@@@@���L����L��@��@@@@���L����L��@��#@@@���������"/.���L����L��@�@@@��@������"+.���L����L��@�@@@��@����"x1���L����L��@�@@@��@����"x2���L����L��@�@@@@���L����L��@��@@@��@���"2.@���L����L��@@@@�@@@�������"/.���L����L��@�@@@��@������>�� L���L��@�@@@��@����"y1��L���L��@�@@@��@����"y2��L���L��@�@@@@��L���L��@��@@@��@���"2.@��%L���&L��@@@@�@@@@�L@@@��A@@���)ocaml.doc��������	� [center_from_bounds] calcule le centre d'une case du quadtree à partir de ses coordonnées
    @param b les coordonnées des coins inférieur gauche et supérieur droit de la case
    @return le centre de la case ��7I � ��8K�@@@@@@@��:L��@@�@���@�����%to_nw��FP\`�GP\e@�@@@��@@���!b��PP\f�QP\g@�@@@��@�����������"x1��bQjq�cQjs@�@@@����"y1��kQju�lQjw@�@@@@��oQjp�pQjx@��@@@�����@��yQj{�zQj|@@@��@��~Qj~�Qj@@@@���Qjz��Qj�@��@@@@�@@@����!b���Qj���Qj�@�@@@@���Qjl@@��@��������"cx���R����R��@�@@@����"cy���R����R��@�@@@@�@@@������2center_from_bounds���R����R��@�@@@��@����!b���R����R��@�@@@@�@@@@���R��@@����������"x1���S����S��@�@@@�����"y1���S����S��@�@@@@���S����S��@��@@@��������"cx���S����S��@�@@@�����"cy���S����S��@�@@@@���S����S��@��@@@@�!@@@�=@@@�p@@@��A@@@��P\\	@@�
@���@�����%to_ne��U���U��@�@@@��@@���!b��U���U��@�@@@��@���������@��'V���(V��@@@����"y1��/V���0V��@�@@@@��3V���4V��@��@@@�������"x2��@V���AV��@�@@@��@��FV���GV��@@@@��IV���JV��@��@@@@�@@@����!b��TV���UV��@�@@@@��XV��@@��@��������"cx��eW���fW��@�@@@����"cy��nW���oW��@�@@@@�@@@������2center_from_bounds��zW� �{W�@�@@@��@����!b���W���W�@�@@@@�@@@@���W��@@����������"cx���X��X@�@@@�����"y1���X��X!@�@@@@���X��X"@��@@@��������"x2���X%��X'@�@@@�����"cy���X)��X+@�@@@@���X$��X,@��@@@@�!@@@�=@@@�p@@@��A@@@���U��	@@�
@���@�����%to_sw���Z.2��Z.7@�@@@��@@���!b���Z.8��Z.9@�@@@��@�����������"x1���[<C��[<E@�@@@��@���[<G��[<H@@@@���[<B��[<I@��@@@�����@��[<L�[<M@@@����"y2��[<O�[<Q@�@@@@��[<K�[<R@��@@@@�@@@����!b��[<U�[<V@�@@@@�� [<>@@��@��������"cx��-\Z`�.\Zb@�@@@����"cy��6\Zd�7\Zf@�@@@@�@@@������2center_from_bounds��B\Zi�C\Z{@�@@@��@����!b��M\Z|�N\Z}@�@@@@�@@@@��R\Z\@@����������"x1��_]���`]��@�@@@�����"cy��i]���j]��@�@@@@��m]���n]��@��@@@��������"cx��{]���|]��@�@@@�����"y2���]����]��@�@@@@���]����]��@��@@@@�!@@@�=@@@�p@@@��A@@@���Z..	@@�
@���@�����%to_se���_����_��@�@@@��@@���!b���_����_��@�@@@��@���������@���`����`��@@@��@���`����`��@@@@���`����`��@��@@@�������"x2���`����`��@�@@@����"y2���`����`��@�@@@@���`����`��@��@@@@�@@@����!b���`����`��@�@@@@���`��@@��@��������"cx���a����a��@�@@@����"cy���a����a��@�@@@@�@@@������2center_from_bounds��
a���a��@�@@@��@����!b��a���a��@�@@@@�@@@@��a��@@����������"cx��'b���(b��@�@@@�����"cy��1b���2b��@�@@@@��5b���6b��@��@@@��������"x2��Cb���Db��@�@@@�����"y2��Mb���Nb��@�@@@@��Qb���Rb��@��@@@@�!@@@�=@@@�p@@@��A@@@��Z_��	@@�
@���A�    �!t��dg%-�eg%.@����!a��lg%*�mg%,@@@B@@��Р%Empty��th15�uh1:@������%bound��~h1>�h1C@@�@@@@@���h13@@�Р$Leaf���iDH��iDL@������%bound���iDP��iDU@@�@@@�����%coord���iDX��iD]@@�@@@���!a���iD`��iDb@@@@@���iDF@@�Р$Node���jcg��jck@������%bound���jco��jct@@�@@@�����!t���jcz��jc{@���!a���jcw��jcy@@@@�	@@@�����!t���jc���jc�@���!a���jc~��jc�@@@@�	@@@�����!t���jc���jc�@���!a���jc���jc�@@@@�	@@@�����!t���jc���jc�@���!a���jc���jc�@@@@�	@@@@@���jce@@@A@����ϐ������2 type du quadtree ��f�f$@@@@@@@��g%%@@�@���@�����%empty��n���n��@�@@@��@@���!b��%n���&n��@�@@@����%Empty��.n���/n��@�����!b��7n���8n��@�@@@�@@@�A@@����������	R [empty] crée un quadtree vide
    @param b les coordonnées de la case initiale ��Hl���Im��@@@@@@@��Kn��@@�@���A�������#get��Yt���Zt��@�@@@��@��@����!t��ft���gt��@���!a��mt���nt��@@@@�	@@@��@����%coord��xt���yt��@@�@@@����&option���t����t��@���!a���t����t��@@@@�	@@@�
@@@� @@@�!A@@�6A@@�  ��@@���!t���u����u��@�@@@��@@������!x���u����u��@�@@@����!y���u����u��@�@@@@���u����u��@��@@@������!t���v����v��@�@@@������%Empty���w����w��@��@���w����w��@@@�@@@@����$None���w���w�@@�@@@������$Leaf���x��x@�����@���x��x@@@��@���x��x@@@����!v���x��x@�@@@@���x��x@��@@@�@@@@����$Some��x�x!@�����!v��x"�x#@�@@@�@@@������$Node��y$(�y$,@�������!b��&y$.�'y$/@�@@@����"q1��/y$1�0y$3@�@@@����"q2��8y$5�9y$7@�@@@����"q3��Ay$9�By$;@�@@@����"q4��Jy$=�Ky$?@�@@@@��Ny$-�Oy$@@��,@@@�8@@@@��@��������"cx��_zDL�`zDN@�@@@����"cy��hzDP�izDR@�@@@@�@@@������2center_from_bounds��tzDU�uzDg@�@@@��@����!b��zDh��zDi@�@@@@�@@@@���zDH@@��������!<���{mv��{mw@�@@@��@����!x���{mt��{mu@�@@@��@����"cx���{mx��{mz@�@@@@�@@@��������$���|{���|{�@�@@@��@����!y���|{���|{�@�@@@��@����"cy���|{���|{�@�@@@@�@@@������#get���|{���|{�@�@@@��@����"q1���|{���|{�@�@@@��@�������!x���|{���|{�@�@@@�����!y���|{���|{�@�@@@@���|{���|{�@��@@@@�,@@@�������#get��	|{��
|{�@�@@@��@����"q2��|{��|{�@�@@@��@�������!x��"|{��#|{�@�@@@�����!y��,|{��-|{�@�@@@@��0|{��1|{�@��@@@@�,@@@��6|{�@@@������������A}���B}��@�@@@��@����!y��L}���M}��@�@@@��@����"cy��W}���X}��@�@@@@�@@@������#get��c~���d~��@�@@@��@����"q3��n~���o~��@�@@@��@�������!x��|~���}~��@�@@@�����!y���~����~��@�@@@@���~����~��@��@@@@�,@@@�������#get���������@�@@@��@����"q4���������@�@@@��@�������!x���������@�@@@�����!y���������@�@@@@���������@��@@@@�,@@@���}��@@@���{mq@@@�E	@@@@���v��@@@�A@@���u��@@@��@����po���lk@@@h@@��@����gf@c@@����ba���^]@@@Z@@Y@@X@@��%A@@������������	� [get] récupère la valeur associée à une coordonnée
    @param t le quadtree
    @param c la coordonnée
    @return la valeur associée à la coordonnée ���p��sr�@@@@@@@���t��5@@�6@���A�������&insert�� F��� F��@�@@@��@��@����!t�� F��� F��@���!a�� F��� F��@@@@�	@@@��@����%coord��! F���" F��@@�@@@��@��!a��* F���+ F��@@@����!t��2 F���3 F��@���!a��9 F���: F��@@@@�	@@@�
@@@�@@@�)@@@�*A@@�?A@@�  ��@@���!t��J G���K G��@�@@@��@@���!c��T G���U G��@�@@@��@@���!v��^ G���_ G��@�@@@������!t��i H���j H��@�@@@������%Empty��t I �u I 	@����!b��| I 
�} I @�@@@�@@@@����$Leaf��� I �� I @��������!b��� I �� I @�@@@�����!c��� I �� I @�@@@�����!v��� I �� I @�@@@@��� I �� I @��@@@�)@@@������$Leaf��� J"�� J&@�������!b��� J(�� J)@�@@@����"oc��� J+�� J-@�@@@����"ov��� J/�� J1@�@@@@��� J'�� J2@��@@@�&@@@@��������!=��� K6?�� K6@@�@@@��@����!c��� K6=�� K6>@�@@@��@����"oc��� K6A�� K6C@�@@@@�@@@����$Leaf��	 LDM�	 LDQ@��������!b��	 LDS�	 LDT@�@@@�����!c��	 LDV�	 LDW@�@@@�����!v��	' LDY�	( LDZ@�@@@@��	+ LDR�	, LD[@��@@@�)@@@���@�����"nt��	: Ngq�	; Ngs@�@@@����$Node��	C Ov~�	D Ov�@��������!b��	O Ov��	P Ov�@�@@@�����%Empty��	Y Ov��	Z Ov�@�������%to_nw��	d Ov��	e Ov�@�@@@��@����!b��	o Ov��	p Ov�@�@@@@��	s Ov��	t Ov�@��@@@�@@@�����%Empty��	 Ov��	� Ov�@�������%to_sw��	� Ov��	� Ov�@�@@@��@����!b��	� Ov��	� Ov�@�@@@@��	� Ov��	� Ov�@��@@@�@@@�����%Empty��	� Ov��	� Ov�@�������%to_ne��	� Ov��	� Ov�@�@@@��@����!b��	� Ov��	� Ov�@�@@@@��	� Ov��	� Ov�@��@@@�@@@�����%Empty��	� Ov��	� Ov�@�������%to_se��	� Ov��	� Ov�@�@@@��@����!b��	� Ov��	� Ov�@�@@@@��	� Ov��	� Ov�@��@@@�@@@@��	� Ov��	� Ov�@���	@@@��@@@@��	� Ngm@@��@�����"nt��	� Q���	� Q��@�@@@������&insert��
 Q���
 Q��@�@@@��@����"nt��
 Q���
 Q��@�@@@��@����"oc��
 Q���
 Q��@�@@@��@����"ov��
' Q���
( Q��@�@@@@�%@@@@��
, Q��@@������&insert��
5 R���
6 R�	@�@@@��@����"nt��
@ R�	�
A R�	@�@@@��@����!c��
K R�	�
L R�	@�@@@��@����!v��
V R�	�
W R�		@�@@@@�%@@@�/@@@��
\ M\e�
] R�	
@��o	@@@��
a K6:@@@������$Node��
j S		�
k S		@�������!b��
u S		�
v S		@�@@@����"q1��
~ S		�
 S		@�@@@����"q2��
� S		�
� S		@�@@@����"q3��
� S		 �
� S		"@�@@@����"q4��
� S		$�
� S		&@�@@@@��
� S		�
� S		'@��,@@@�8@@@@��@��������!x��
� T	+	3�
� T	+	4@�@@@����!y��
� T	+	6�
� T	+	7@�@@@@�@@@����!c��
� T	+	:�
� T	+	;@�@@@@��
� T	+	/@@��@��������"cx��
� U	?	G�
� U	?	I@�@@@����"cy��
� U	?	K�
� U	?	M@�@@@@�@@@������2center_from_bounds��
� U	?	P�
� U	?	b@�@@@��@����!b��
� U	?	c�
� U	?	d@�@@@@�@@@@��
� U	?	C@@�����������v�� V	h	u� V	h	v@�@@@��@����!x�� V	h	s� V	h	t@�@@@��@����"cx�� V	h	w� V	h	y@�@@@@�@@@����������& V	h	}�' V	h	~@�@@@��@����!y��1 V	h	{�2 V	h	|@�@@@��@����"cy��< V	h	�= V	h	�@�@@@@�@@@@�2@@@���������$true��L W	�	��M W	�	�@@�@@@�����
��U W	�	��V W	�	�@@�@@@@�@@@@����$Node��_ W	�	��` W	�	�@��������!b��k W	�	��l W	�	�@�@@@�������&insert��w W	�	��x W	�	�@�@@@��@����"q1��� W	�	��� W	�	�@�@@@��@����!c��� W	�	��� W	�	�@�@@@��@����!v��� W	�	��� W	�	�@�@@@@�%@@@�����"q2��� W	�	��� W	�	�@�@@@�����"q3��� W	�	��� W	�	�@�@@@�����"q4��� W	�	��� W	�	�@�@@@@��� W	�	��� W	�	�@��T@@@�a@@@������������ X	�	��� X	�	�@@�@@@�����%false��� X	�	��� X	�	�@@�@@@@�@@@@����$Node��� X	�	��� X	�	�@��������!b��� X	�	��� X	�	�@�@@@�����"q1��� X	�	��� X	�	�@�@@@�������&insert��  X	�	�� X	�	�@�@@@��@����"q2�� X	�	�� X	�	�@�@@@��@����!c�� X	�	�� X	�	�@�@@@��@����!v��! X	�	��" X	�	�@�@@@@�%@@@�����"q3��, X	�	��- X	�	�@�@@@�����"q4��6 X	�	��7 X	�	�@�@@@@��: X	�	��; X	�	�@��T@@@�a@@@���������v��I Y	�
�J Y	�
@@�@@@�������R Y	�
�S Y	�
@@�@@@@�@@@@����$Node��\ Y	�
�] Y	�
@��������!b��h Y	�
�i Y	�
@�@@@�����"q1��r Y	�
�s Y	�
@�@@@�����"q2��| Y	�
�} Y	�
@�@@@�������&insert��� Y	�
!�� Y	�
'@�@@@��@����"q3��� Y	�
(�� Y	�
*@�@@@��@����!c��� Y	�
+�� Y	�
,@�@@@��@����!v��� Y	�
-�� Y	�
.@�@@@@�%@@@�����"q4��� Y	�
0�� Y	�
2@�@@@@��� Y	�
�� Y	�
3@��T@@@�a@@@������������� Z
4
;�� Z
4
@@@�@@@��������� Z
4
B�� Z
4
G@@�@@@@�@@@@����$Node��� Z
4
K�� Z
4
O@��������!b��� Z
4
Q�� Z
4
R@�@@@�����"q1��� Z
4
T�� Z
4
V@�@@@�����"q2��� Z
4
X�� Z
4
Z@�@@@�����"q3�� Z
4
\� Z
4
^@�@@@�������&insert�� Z
4
`� Z
4
f@�@@@��@����"q4�� Z
4
g� Z
4
i@�@@@��@����!c��& Z
4
j�' Z
4
k@�@@@��@����!v��1 Z
4
l�2 Z
4
m@�@@@@�%@@@@��6 Z
4
P�7 Z
4
n@��T@@@�a@@@@��< V	h	l�= Z
4
o@���@ V	h	m
@@@�K@@@�~@@@@��D H��@@@��	A@@��
A@@��H G��@@@��@����BA���>=@@@:@@��@����98@5@@��@��43@@����0/���,+@@@(@@'@@&@@%@@�b'A@@���:3�������	� [insert] insère une valeur dans le quadtree
    @param t le quadtree
    @param c la coordonnée
    @param v la valeur à insérer
    @return le quadtree avec la valeur insérée ��p A���q E�@@@@@@@��s F��7@@�8@���@�����-prune_non_rec�� _�� _ @�@@@��@@���!t��� _!�� _"@�@@@������!t��� `%-�� `%.@�@@@������%Empty��� a48�� a4=@��@��� a4>�� a4?@@@�@@@@����!t��� a4C�� a4D@�@@@������$Leaf��� bEI�� bEM@��@��� bEN�� bEO@@@�@@@@����!t��� bES�� bET@�@@@��������$Node��� cUY�� cU]@�������!b��� cU_�� cU`@�@@@����"q1��� cUb�� cUd@�@@@����"q2��� cUf�� cUh@�@@@����"q3��� cUj�� cUl@�@@@����"q4�� cUn� cUp@�@@@@�� cU^� cUq@��,@@@�8@@@�!n�� cUu� cUv@�>@@@@���������"q1�� dz�� dz�@�@@@�����"q2��& dz��' dz�@�@@@�����"q3��0 dz��1 dz�@�@@@�����"q4��: dz��; dz�@�@@@@�"@@@���������%Empty��I e���J e��@��@��N e���O e��@@@�@@@�����%Empty��X e���Y e��@��@��] e���^ e��@@@�@@@�����%Empty��g e���h e��@��@��l e���m e��@@@�@@@�����%Empty��v e���w e��@��@��{ e���| e��@@@�@@@@�6@@@@����%Empty��� e���� e��@�����!b��� e���� e��@�@@@�@@@���������$Leaf��� f���� f��@�����@��� f���� f��@@@����!c��� f���� f��@�@@@����!v��� f���� f��@�@@@@��� f���� f��@��@@@�"@@@�����%Empty��� f���� f��@��@��� f���� f��@@@�@@@�����%Empty��� f���� f��@��@��� f���� f��@@@�@@@�����%Empty��� f���� f��@��@��� f���� f��@@@�@@@@�P@@@@����$Leaf��� f��� f�@��������!b��� f��  f�	@�@@@�����!c��	 f��
 f�@�@@@�����!v�� f�� f�@�@@@@�� f�� f�@��@@@�)@@@���������%Empty��' g�( g@��@��, g�- g@@@�@@@�����$Leaf��6 g!�7 g%@�����@��> g'�? g(@@@����!c��F g*�G g+@�@@@����!v��O g-�P g.@�@@@@��S g&�T g/@��@@@�"@@@�����%Empty��_ g1�` g6@��@��d g7�e g8@@@�@@@�����%Empty��n g:�o g?@��@��s g@�t gA@@@�@@@@�P@@@@����$Leaf��} gE�~ gI@��������!b��� gK�� gL@�@@@�����!c��� gN�� gO@�@@@�����!v��� gQ�� gR@�@@@@��� gJ�� gS@��@@@�)@@@���������%Empty��� hT[�� hT`@��@��� hTa�� hTb@@@�@@@�����%Empty��� hTd�� hTi@��@��� hTj�� hTk@@@�@@@�����$Leaf��� hTm�� hTq@�����@��� hTs�� hTt@@@����!c��� hTv�� hTw@�@@@����!v��� hTy�� hTz@�@@@@��� hTr�� hT{@��@@@�"@@@�����%Empty��� hT}�� hT�@��@��� hT��� hT�@@@�@@@@�P@@@@����$Leaf�� hT�� hT�@��������!b�� hT�� hT�@�@@@�����!c�� hT�� hT�@�@@@�����!v��' hT��( hT�@�@@@@��+ hT��, hT�@��@@@�)@@@���������%Empty��; i���< i��@��@��@ i���A i��@@@�@@@�����%Empty��J i���K i��@��@��O i���P i��@@@�@@@�����%Empty��Y i���Z i��@��@��^ i���_ i��@@@�@@@�����$Leaf��h i���i i��@�����@��p i���q i��@@@����!c��x i���y i��@�@@@����!v��� i���� i��@�@@@@��� i���� i��@��@@@�"@@@@�P@@@@����$Leaf��� i���� i��@��������!b��� i���� i��@�@@@�����!c��� i���� i��@�@@@�����!v��� i���� i��@�@@@@��� i���� i��@��@@@�)@@@���@��� j���� j��@@@@����!n��� j���� j��@�@@@@��� dz~�� j��@���� dz@@@@��� `%'@@@�IA@@������������	� [prune_non_rec] supprime les sous-cases vides d'un noeud non récursivement
    @param t le quadtree
    @return le quadtree sans les sous-cases vides ��� \
q
q�� ^
�@@@@@@@��� _@@�@���A�����&remove��� p���� p��@�@@@��@@���!t��� p���� p��@�@@@��@@������!x�� p��� p��@�@@@����!y�� p��� p��@�@@@@�� p��� p��@��@@@������!t�� q��� q��@�@@@������%Empty��( r���) r��@��@��- r���. r��@@@�@@@@����!t��6 r���7 r��@�@@@������$Leaf��A s���B s��@�������!b��L s���M s��@�@@@��@��R s���S s��@@@��@��W s���X s��@@@@��Z s���[ s��@��@@@�@@@@����%Empty��e s���f s��@�����!b��n s���o s��@�@@@�@@@������$Node��z t���{ t��@�������!b��� t���� t��@�@@@����"q1��� t���� t��@�@@@����"q2��� t���� t��@�@@@����"q3��� t���� t��@�@@@����"q4��� t���� t�@�@@@@��� t���� t�@��,@@@�8@@@@��@��������"cx��� u�� u@�@@@����"cy��� u�� u@�@@@@�@@@������2center_from_bounds��� u�� u)@�@@@��@����!b��� u*�� u+@�@@@@�@@@@��� u
@@�����������b��� v/<�� v/=@�@@@��@����!x��� v/:�� v/;@�@@@��@����"cx�� v/>� v/@@�@@@@�@@@���������� v/D� v/E@�@@@��@����!y�� v/B� v/C@�@@@��@����"cy��( v/F�) v/H@�@@@@�@@@@�2@@@�����������7 wNU�8 wNY@@�@@@��������@ wN[�A wN_@@�@@@@�@@@@������-prune_non_rec��L wNc�M wNp@�@@@��@����$Node��W wNr�X wNv@��������!b��c wNx�d wNy@�@@@�������&remove��o wN{�p wN�@�@@@��@����"q1��z wN��{ wN�@�@@@��@�������!x��� wN��� wN�@�@@@�����!y��� wN��� wN�@�@@@@��� wN��� wN�@��@@@@�,@@@�����"q2��� wN��� wN�@�@@@�����"q3��� wN��� wN�@�@@@�����"q4��� wN��� wN�@�@@@@��� wNw�� wN�@��[@@@��� wNq�� wN�@��l@@@@�x@@@������������� x���� x��@@�@@@�������� x���� x��@@�@@@@�@@@@������-prune_non_rec��� x���� x��@�@@@��@����$Node��� x���� x��@��������!b��� x���� x��@�@@@�����"q1�� x��� x��@�@@@�������&remove�� x��� x��@�@@@��@����"q2�� x��� x��@�@@@��@�������!x��) x���* x��@�@@@�����!y��3 x���4 x��@�@@@@��7 x���8 x��@��@@@@�,@@@�����"q3��C x���D x��@�@@@�����"q4��M x���N x��@�@@@@��Q x���R x��@��[@@@��V x���W x��@��l@@@@�x@@@������������e y���f y��@@�@@@�����#��n y���o y��@@�@@@@�@@@@������-prune_non_rec��z y���{ y�
@�@@@��@����$Node��� y��� y�@��������!b��� y��� y�@�@@@�����"q1��� y��� y�@�@@@�����"q2��� y��� y�@�@@@�������&remove��� y��� y�#@�@@@��@����"q3��� y�$�� y�&@�@@@��@�������!x��� y�(�� y�)@�@@@�����!y��� y�+�� y�,@�@@@@��� y�'�� y�-@��@@@@�,@@@�����"q4��� y�/�� y�1@�@@@@��� y��� y�2@��[@@@��� y��� y�3@��l@@@@�x@@@���������)��� z4;�� z4@@@�@@@�����2�� z4B� z4G@@�@@@@�@@@@������-prune_non_rec�� z4K� z4X@�@@@��@����$Node�� z4Z� z4^@��������!b��( z4`�) z4a@�@@@�����"q1��2 z4c�3 z4e@�@@@�����"q2��< z4g�= z4i@�@@@�����"q3��F z4k�G z4m@�@@@�������&remove��R z4o�S z4u@�@@@��@����"q4��] z4v�^ z4x@�@@@��@�������!x��k z4z�l z4{@�@@@�����!y��u z4}�v z4~@�@@@@��y z4y�z z4@��@@@@�,@@@@�� z4_�� z4�@��[	@@@��� z4Y�� z4�@��l@@@@�x@@@@��� v/3�� z4�@���� v/4
@@@��@@@@��� q��@@@��A@@��	A@@���jc�������	� [remove] supprime un objet du quadtree en élaguant si nécessaire
    @param t le quadtree
    @param c la coordonnée
    @return le quadtree sans l'objet ��� l���� of�@@@@@@@��� p��@@�@���A�����(iter_val��� 
�� 
@�@@@��@@���!t��� 
�� 
@�@@@��@@���!f��� 
�� 
@�@@@������!t��� �!)�� �!*@�@@@������%Empty��� �04�� �09@��@��� �0:�� �0;@@@�@@@@����"()��� �0?�� �0A@@�@@@������$Leaf��� �BF�� �BJ@�����@��� �BL�� �BM@@@��@��� �BO�  �BP@@@����!v�� �BR� �BS@�@@@@�� �BK� �BT@��@@@�@@@@������!f�� �BX� �BY@�@@@��@����!v��# �BZ�$ �B[@�@@@@�@@@������$Node��/ �\`�0 �\d@�����@��7 �\f�8 �\g@@@����"q1��? �\i�@ �\k@�@@@����"q2��H �\m�I �\o@�@@@����"q3��Q �\q�R �\s@�@@@����"q4��Z �\u�[ �\w@�@@@@��^ �\e�_ �\x@��+@@@�4@@@@�  ������(iter_val��m �|��n �|�@�@@@��@����"q1��x �|��y �|�@�@@@��@����!f��� �|��� �|�@�@@@@�@@@�  ������(iter_val��� ����� ���@�@@@��@����"q2��� ����� ���@�@@@��@����!f��� ����� ���@�@@@@�@@@�  ������(iter_val��� ����� ���@�@@@��@����"q3��� ����� ���@�@@@��@����!f��� ����� ���@�@@@@�@@@������(iter_val��� ����� ���@�@@@��@����"q4��� ����� ���@�@@@��@����!f��� ����� ���@�@@@@�@@@�=@@@�b@@@��@@@@��� �!#@@@�4	A@@�?
A@@����ǐ������	� [iter_val] applique une fonction à tous les objets du quadtree
    @param t le quadtree
    @param f la fonction à appliquer �� |��� ~�	@@@@@@@�� 

@@�@���@�����8filter_val_count_removal�� ���� ���@�@@@��@@���!t�� ���� ���@�@@@��@@���!f��' ����( ���@�@@@��A�����#aux��3 ����4 ���@�@@@��@@���!t��= ����> ���@�@@@��@@���#acc��G ����H ���@�@@@������!t��R ����S ���@�@@@������%Empty��] ���^ ��
@��@��b ���c ��@@@�@@@@�������!t��n ���o ��@�@@@�����#acc��x ���y ��@�@@@@�@@@������$Leaf��� ��� �!@�������!b��� �#�� �$@�@@@��@��� �&�� �'@@@����!v��� �)�� �*@�@@@@��� �"�� �+@��@@@�"@@@@��������!f��� �2�� �3@�@@@��@����!v��� �4�� �5@�@@@@�@@@�������!t��� �;�� �<@�@@@�����#acc��� �>�� �A@�@@@@�@@@��������%Empty��� �G�� �L@�����!b��� �M�� �N@�@@@�@@@�������!+��� �T�� �U@�@@@��@����#acc�� �P� �S@�@@@��@���!1@�� �V� �W@@@@�@@@@�/@@@�� �/@@@������$Node�� �X^� �Xb@�������!b��$ �Xd�% �Xe@�@@@����"q1��- �Xg�. �Xi@�@@@����"q2��6 �Xk�7 �Xm@�@@@����"q3��? �Xo�@ �Xq@�@@@����"q4��H �Xs�I �Xu@�@@@@��L �Xc�M �Xv@��,@@@�8@@@@��@��������"q1��] �z��^ �z�@�@@@����$nacc��f �z��g �z�@�@@@@�@@@������#aux��r �z��s �z�@�@@@��@����"q1��} �z��~ �z�@�@@@��@����#acc��� �z��� �z�@�@@@@�@@@@��� �z�@@��@��������"q2��� ����� ���@�@@@����$nacc��� ����� ���@�@@@@�@@@������#aux��� ����� ���@�@@@��@����"q2��� ����� ���@�@@@��@����$nacc��� ����� ���@�@@@@�@@@@��� ���@@��@��������"q3��� ����� ���@�@@@����$nacc��� ����� ���@�@@@@�@@@������#aux��� ����� ���@�@@@��@����"q3��� ����� ���@�@@@��@����$nacc�� ���� ���@�@@@@�@@@@�� ���@@��@��������"q4�� ���� ���@�@@@����$nacc�� ���� ���@�@@@@�@@@������#aux��) ����* ���@�@@@��@����"q4��4 ����5 �� @�@@@��@����$nacc��? ���@ ��@�@@@@�@@@@��D ���@@���������-prune_non_rec��P �	�Q �	@�@@@��@����$Node��[ �	�\ �	"@��������!b��g �	$�h �	%@�@@@�����"q1��q �	'�r �	)@�@@@�����"q2��{ �	+�| �	-@�@@@�����"q3��� �	/�� �	1@�@@@�����"q4��� �	3�� �	5@�@@@@��� �	#�� �	6@��0@@@��� �	�� �	7@��A@@@@�M@@@�����$nacc��� �	9�� �	=@�@@@@�X@@@�e@@@��@@@��@@@�@@@@��� ���	@@@�h
A@@�sA@@@��� ���@@������#aux��� �CE�� �CH@�@@@��@����!t��� �CI�� �CJ@�@@@��@���!0@��� �CK�� �CL@@@@�@@@�"@@@��A@@��A@@������������	� [fold_val] filtre selon un prédicat et compte les objets enlevés
    @param t le quadtree
    @param f le prédicat
    @return
      le quadtree sans les objets qui ne vérifient pas le prédicat et le nombre d'objets enlevés ��� ����� �N�@@@@@@@��� ���@@�@���@�������  A@@@�������)unset_lib@@��@���@@@@@@@@@