<%@ Page Language="c#" AutoEventWireup="true" Inherits="Logindemo" Codebehind="Login.aspx.cs" %>
<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.0 Transitional//EN">
<html>
<head>
	<meta http-equiv="Content-Type" content="text/html; charset=UTF-8" />
	<meta name="viewport" content="width=1,initial-scale=1,user-scalable=1" />
	<title>Insert title here</title>
	
	<link href="http://fonts.googleapis.com/css?family=Lato:100italic,100,300italic,300,400italic,400,700italic,700,900italic,900" rel="stylesheet" type="text/css">
	<link rel="stylesheet" type="text/css" href="assets/bootstrap/css/bootstrap.min.css" />
	<link rel="stylesheet" type="text/css" href="assets/css/styles.css" />
	
	<!-- HTML5 Shim and Respond.js IE8 support of HTML5 elements and media queries -->
    <!--[if lt IE 9]>
      <script src="https://oss.maxcdn.com/html5shiv/3.7.2/html5shiv.min.js"></script>
      <script src="https://oss.maxcdn.com/respond/1.4.2/respond.min.js"></script>
    <![endif]-->
</head>
<body>

	<section class="container">
			<section class="login-form">
				<form method="post" action="" role="login">
					<img src="http://www.advantzware.com/wp-content/uploads/2016/09/advantzware-logo8.png" class="img-responsive" alt="" />
                    <input type="text" name="name" placeholder="User Name" required class="form-control input-lg" />
					<input type="password" name="password" placeholder="Password" required class="form-control input-lg" />
					<input type="checkbox" name="checkbox"  > Remember Password 
					<button type="button" class="btn btn-lg btn-primary btn-block" onclick="cmdLogin_Click()"  > Sign in</button>
					<%--<button type="submit" name="go" class="btn btn-lg btn-primary btn-block" onclick="location.href='menu.html'"> Sign in</button>--%>
                     
					<div>
						<a href="#"></a>  <a href="#">forgot password</a>
					</div>
				</form>
				<div class="form-links">
					<a href="http://www.advantzware.com">www.advantzware.com</a>
				</div>
			</section>
	</section>
	
	<script src="http://ajax.googleapis.com/ajax/libs/jquery/2.1.1/jquery.min.js"></script>
	<script src="assets/bootstrap/js/bootstrap.min.js"></script>
	<script type="text/javascript" >
	    function cmdLogin_Click() {
	        window.location.href = "menu.html";
	    }
	</script>
</body>
</html>