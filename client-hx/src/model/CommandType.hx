package model;

enum CommandType {
	RegisterUser;
	Login;
	QueryUser;
	DeleteUser;
	UpdateUser;
	CreateUserTerms;
	UpdateUserTerms;
	QueryUserTerms;
	DeleteUserTerms;
	CreateUserPreferences;
	UpdateUserPreferences;
	QueryUserPreferences;
	DeleteUserPreferences;
	CCARUpload;
	ParsedCCARText;
	Undefined;
}