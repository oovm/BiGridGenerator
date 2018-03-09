BeginPackage["ZhihuLink`"];
ZhihuLink::usage = "ZhihuLink";
(* ::Section:: *)
(*程序包正体*)
(* ::Subsection::Closed:: *)
(*主设置*)
$ZhihuCookies::usage = "知乎Cookies, 有效期约一个月.";
ZhihuStatsGet::usage = "";
ZhihuFollowees::usage = "ZhihuFollowees[id] 获取用户的关注者数据.";
Begin["`Private`"];
(* ::Subsection::Closed:: *)
(*主体代码*)
ZhihuLink$Version="V1.0";
ZhihuLink$LastUpdate="2016-11-11";
(* ::Subsubsection:: *)
(*Keys*)
$ZhihuCookies="aliyungf_tc=AQAAABY6ZDrdMwYAyhPfPEbyzrk9hC4+; _xsrf=0ff13a61-e50b-47a2-b7c8-6282ab4be718; q_c1=ec74bc715a2144e2969b334b09266782|1520162956000|1520162956000; _zap=cf8bf686-6ea8-40b8-9290-eb017f15681b; capsion_ticket=\"2|1:0|10:1520162959|14:capsion_ticket|44:YWVlMDQ5OTY4NTkwNGQ0Zjk2NjExNDRkNmVkZGJiYWQ=|e4b5f23bf2a0ce29bbc5d7bf876ff6b410d876f7e0425e8884bb11f3c6fe4981\"; l_n_c=1; r_cap_id=\"YzJiZWE4N2NjMjY2NGY1MGEyMDNhMDJiOTg3ZWMwN2U=|1520162989|a6166482a73ece5b0941a3ffdb611fc6d33e3dc1\"; cap_id=\"ODYzOTBmMzdkMjJkNDk3Nzk1YzYzYThhMzY3NGIzMGM=|1520162989|955b8025dc40d996a031a5927432de2747c2a067\"; l_cap_id=\"ZmM1ZWY5Y2U4ZDA2NDgyMWI5ZTgxODM1ZjM5NzM5Y2U=|1520162989|7cf5803ea5197df800134523ba120895692b0edb\"; n_c=1; z_c0=\"MS4xSHRpbkJBQUFBQUFYQUFBQVlRSlZUYlltaVZ1ckZDR1JoMk91UFZ6aUk5MllfRm5KUGNCZ0d3PT0=|1520162998|5448132cc2b7caa44c4e81b5845c9656cdbcfaf2\"; d_c0=\"AIBrq01FPA2PTjjXCuBar7KCs7lMTOoHxEo=|1520162998\"";
(*Todo: 千万别忘了删掉*)
name="GalAster";
$keyMin={
	{"name","用户名"},
	{"url_token","ID"},
	{"follower_count","粉丝数"},
	{"voteup_count","获赞数"},
	{"favorited_count","获收藏"},
	{"thanked_count","获感谢"},
	{TimeObject,"时间戳"}
};
(* ::Subsubsection:: *)
(*ZhihuStats*)
ZhihuLink::para="非法参数 `1` !";
Options[ZhihuStatsGet]={Return->Min,Times->True};
ZhihuStatsGet[name_String,OptionsPattern[]]:=Block[
	{needs, get,return},
	Switch[OptionValue[Return],
		Min,
			needs="follower_count,voteup_count,favorited_count,thanked_count",
		Normal,
			needs="follower_count,voteup_count,favorited_count,thanked_count,following_question_count,following_count,
			answer_count,articles_count,question_count,logs_count,favorite_count,following_favlists_count,columns_count,
			pins_count",
		Raw,
			needs="locations,employments,gender,educations,business,voteup_count,thanked_Count,follower_count,
			cover_url,following_topic_count,following_question_count,following_favlists_count,following_columns_count,
			avatar_hue,answer_count,articles_count,pins_count,question_count,columns_count,commercial_question_count,
			favorite_count,favorited_count,logs_count,included_answers_count,included_articles_count,included_text,
			message_thread_token,account_status,is_active,is_bind_phone,is_force_renamed,is_bind_sina,thank_to_count,
			is_privacy_protected,sina_weibo_url,sina_weibo_name,show_sina_weibo,is_blocking,is_blocked,is_following,
			is_followed,is_org_createpin_white_user,mutual_followees_count,vote_to_count,vote_from_count,following_count,
			thank_from_count,thanked_count,description,hosted_live_count,participated_live_count,allow_message,
			industry_category,org_name,org_homepage,badge[?(type=best_answerer)].topics"
	];
	get=URLExecute[HTTPRequest[<|
		"Scheme"->"https",
		"Domain"->"www.zhihu.com",
		"Headers"->{"Cookie"->$ZhihuCookies},
		"Path"->{"api/v4/members",name},
		"Query"->{"include"->needs}
	|>],Authentication->None];
	If[
		Head@get===String,
		Message[ZhihuLink::para,name];
		Return@Missing["NotAvailable"]
	];
	If[OptionValue[Return]===Raw,Return@get];
	return=Switch[OptionValue[Return],
		Min,
			Join[{"url_token","name"},StringSplit[needs,","]]/.get,
		Normal,
			Join[{"url_token","name"},StringSplit[needs,","]]/.get
	];
	If[OptionValue[Times],AppendTo[return,TimeObject[]],return]
];




(* ::Subsubsection:: *)
(*ZhihuFollowees*)
ZhihuFolloweesGet[name_String,offset_Integer]:=Block[
	{get,needs},
	needs="data[*].follower_count,voteup_count,favorited_count,thanked_count";
	get="data"/.URLExecute[HTTPRequest[<|
		"Scheme"->"https",
		"Domain"->"www.zhihu.com",
		"Headers"->{"Cookie"->$ZhihuCookies},
		"Path"->{"api/v4/members",name,"followees"},
		"Query"->{"include"->needs,"offset"->offset,"limit"->offset+20}
	|>],Authentication->None];
	ArrayPad[$keyMin[[1;;6,1]]/.get,{{0},{0,1}},DateObject[]]
];
ZhihuFollowees[name_String]:=Block[
	{count,raw,data},
	count=ZhihuStatsGet[name,Return->Normal,Times->False][[8]];
	Echo[count,"你关注的用户数: "];
	raw=Flatten[ZhihuFolloweesGet[name,20#]&/@Range[0,Quotient[count,20]],1];
	(*TableForm[raw,TableHeadings\[Rule]{None,Last@@@$keyMin}];*)
	data=Association[Rule@@@Transpose@{$keyMin[[All,2]],#}]&/@raw;
	data//Dataset
];


(* ::Subsection::Closed:: *)
(*附加设置*)
End[] ;
SetAttributes[
	{ },
	{Protected,ReadProtected}
];
EndPackage[];
