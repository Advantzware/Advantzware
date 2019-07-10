 
#region " using "
using System;
using System.Web;
using System.Configuration;
#endregion

public partial class Logindemo  : System.Web.UI.Page
{

  protected void Page_Load( object sender,  System.EventArgs e) 
  {
    //Put user code to initialize the page here
    //lblMessage.Text = "";

    if (! Page.IsPostBack ) 
    {
      if ( string.IsNullOrEmpty(ConfigurationManager.AppSettings["LoginMethod"]) || (ConfigurationManager.AppSettings["LoginMethod"]).ToUpper()  == "WITHOUTLOGIN" )
      {
        //Redirect();
        //return;
      }
      //
      if ( Request.QueryString["user"] != null ) 
      {
       // txtUserName.Text = Request.QueryString["user"];
        //txtPassword.Attributes.Add("value","");
      }
      else 
      {
        if ( Request.Browser.Cookies ) 
        {
          if ( Request.Cookies["UserInfo"] != null ) 
          {
           // txtUserName.Text = Request.Cookies["UserInfo"]["UserLogin"];
           // txtPassword.Attributes.Add("value", Request.Cookies["UserInfo"]["UserPwd"]);
            //chbSavePassword.Checked = true;
          }
          //chbSavePassword.Visible = true;
        } 
        else 
        {
          //chbSavePassword.Visible = false;
        }
      }
      //Session.Clear();
    }
    //if ( txtUserName.Text == "" ) Page.SetFocus(txtUserName);
    //else Page.SetFocus(txtPassword);

  }

  protected void cmdLogin_Click( object sender,  System.EventArgs e)  
  {
  
    UserClass  userLogin = new UserClass(); 
    HttpCookie  aCookie = new HttpCookie("UserInfo");
    HttpCookie showCookie = new HttpCookie("showmenu");
    HttpCookie asiCookie = new HttpCookie("asimenu");
    //try 
    //{
    //  //userLogin = userLogin.Login(txtUserName.Text, txtPassword.Text);

    //  if (Request.Cookies["showmenu"] != null)
    //  {
    //      showCookie.Value = "1";          
    //      Response.Cookies.Add(showCookie);          
    //  }
    //  if (Request.Cookies["asimenu"] != null)
    //  {
    //      asiCookie.Expires = DateTime.Now;
    //      Response.Cookies.Add(asiCookie);
    //  }
    //  if (userLogin != null && userLogin.ID != Guid.Empty) 
    //  {
    //    Session["User"] = userLogin;
    //    //MOD-001
    ////    Session["Company"] = getDefaultComp(((UserClass)Session["User"]).UserName.ToString());
    ////    Session["CompanyName"] = getCompName(Session["Company"].ToString());
       
    //    if ( Request.Browser.Cookies ) 
    //    {
    //      if ( chbSavePassword.Checked ) 
    //      {
    //        aCookie["UserLogin"] = txtUserName.Text;
    //        aCookie["UserPwd"] = txtPassword.Text;
            
    //        aCookie.Expires = DateTime.Now.AddYears(1);
    //      } 
    //      else 
    //      {
    //        aCookie.Expires = DateTime.Now;
    //      }
    //      Response.Cookies.Add(aCookie);
          
             
          
    //    }
    //  } 
    //  else 
    //  {
    //    userLogin = null;
    //    Session["User"] = null;
    //    lblMessage.Text = "Invalid Login";
    //  }
    //} 
    //catch
    //{
    //  userLogin = null;
    //  Session["User"] = null;
    //  lblMessage.Text = "Invalid Login";
    //}
    //if (userLogin != null ) 
    //{
         
      //Redirect();
    //}
    HttpContext.Current.Response.Write("<script>alert('" + "dfkjghsldfgh" + "')</script>");
    Response.Redirect("menu.aspx", false);

   // HttpContext.Current.Response.Write("<script>history.back()</script>");
      
    
 }
 
  protected void Redirect() 
  {
    string sStartPageURL = "";
    if ( Request.QueryString["url"] != null ) sStartPageURL = Request.QueryString["url"];
    else sStartPageURL = ConfigurationManager.AppSettings["StartPage"];

    if ( sStartPageURL == "" ) Response.Write("<script language=javascript>alert('" + "Start page isn't set" + "');</script>");
    else Response.Redirect(sStartPageURL);
  }

}