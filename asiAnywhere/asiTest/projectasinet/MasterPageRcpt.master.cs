using System;
using System.Data;
using System.Configuration;
using System.Collections;
using System.Web;
using System.Web.Security;
using System.Web.UI;
using System.Web.UI.WebControls;
using System.Web.UI.WebControls.WebParts;
using System.Web.UI.HtmlControls;

/// <summary>
/// Summary description for Class1
/// </summary>
public partial class MasterRcpt : System.Web.UI.MasterPage
{
    public MasterRcpt()
    {
        //
        // TODO: Add constructor logic here
        //
    }

    protected void Page_Load(object sender, EventArgs e)
    {
        string fname = this.Page.GetType().Name.ToString();
        if (fname == "list_rcpt_aspx")
        {

            lilist_rcpt.Attributes.Add("class", "selected");
        }
        else if (fname == "view_rcpt_aspx")
        {
            liview_rcpt.Attributes.Add("class", "selected");
        }
        else 
        {
            lilist_item.Attributes.Add("class", "selected");
        }
        if (!Page.IsPostBack)
        {

            if (Session["User"] != null)
            {
                UserClass UserLogin = (UserClass)Session["User"];
                lblUser.Text = UserLogin.UserName;


                string vUserId = UserLogin.UserName;
                string vPage = "list_rcpt.aspx";
                string aUsers = null;
                string PrmComp = null;
                bool vCanCreate = false;
                bool vCanRun = false;
                bool vCanUpdate = false;
                bool vCanDelete = false;

                func1 f1 = new func1();
                //Response.Write(Page);
                f1.CheckProgramPermissions(vPage, vUserId, ref  vCanCreate, ref  vCanRun, ref  vCanUpdate, ref  vCanDelete, ref  PrmComp, ref  aUsers);

                lblComp.Text = PrmComp;
                //Response.Write(vCanRun);
                if (vCanRun == true)
                {
                    lnklist_rcpt.Visible = true;
                    //list_rcpt.Visible = true;

                }

                if (vCanRun == false)
                {
                    lnklist_rcpt.Visible = false;
                    //list_rcpt.Visible = false;

                }


                

            }
        }
    }

    protected void LinkButton1_Click(object sender, EventArgs e)
    {
        Response.Redirect("menu.aspx");
    }

    protected void hlnkLogOut_Click(object sender, EventArgs e)
    {
        string sLoginURL = ConfigurationManager.AppSettings["LoginFile"];
        if (sLoginURL == "")
        {
            Response.Write("<script language=javascript>alert('" + "Login page isn’t set" + "!');</script>");
            return;
        }

        Page.Session.Clear();
        if (Request.Cookies["showmenu"] != null)
        {
            Response.Cookies["showmenu"].Expires = DateTime.Now.AddDays(-1);
        }
        Response.Redirect(sLoginURL);
    }

    protected void list_rcpt_Click(object sender, EventArgs e)
    {
        Response.Redirect("list_rcpt.aspx");
    }

    protected void view_rcpt_Click(object sender, EventArgs e)
    {
        
        Response.Redirect("view_rcpt.aspx");
    }
   
    
    
    protected void lnk_listitem_click(object sender, EventArgs e)
    {
       Response.Redirect("list_set_part.aspx");
    }

    
    protected void img_btn_add_click(object sender, ImageClickEventArgs e)
    {
        Session["add_rcpt_list_buton"] = "Additem";
        Response.Redirect("view_rcpt.aspx");

    }
    protected void img_btn_exit_click(object sender, EventArgs e)
    {
        string sLoginURL = ConfigurationManager.AppSettings["LoginFile"];
        if (sLoginURL == "")
        {
            Response.Write("<script language=javascript>alert('" + "Login page isn’t set" + "!');</script>");
            return;
        }

        Page.Session.Clear();
        if (Request.Cookies["showmenu"] != null)
        {
            Response.Cookies["showmenu"].Expires = DateTime.Now.AddDays(-1);
        }
        Response.Redirect(sLoginURL);
    }
}