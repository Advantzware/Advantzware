 
#region " using "
using System;
using System.Configuration;
#endregion

public partial class MenuDemo : System.Web.UI.Page
{

  void Page_Load( object sender,  System.EventArgs e) 
{
        //Put user code to initialize the page here

        UserClass.CheckLogin(Page);
        if (Session ["User"] != null)
        {
            UserClass UserLogin = (UserClass)Session["User"]; 
            lblUser.Text = UserLogin.UserName;
        }
         //

//
 
      // hlnkPrint_usergroup.Visible = func.CheckUserPermissions("[dbo].[usergroup]", "SA");
 
      // hlnkPrint_main_menu.Visible = func.CheckUserPermissions("[dbo].[main_menu]", "SA");
 
      // hlnkPrint_user_master.Visible = func.CheckUserPermissions("[dbo].[user_master]", "SA");

//
        
      
}

protected void hlnkLogOut_Click( object sender,  System.EventArgs e)  
{
     string sLoginURL = ConfigurationManager.AppSettings["LoginFile"];
     if ( sLoginURL == "" ) 
     {
            Response.Write("<script language=javascript>alert(' " + "Login page isn’t set"+"');</script>");
            return;
     }
     Session["User"] = null;
     Session.Clear();
     if (Request.Cookies["showmenu"] != null)
     {
         Response.Cookies["showmenu"].Expires = DateTime.Now.AddDays(-1);
     }
     Response.Redirect(sLoginURL);
 }

}