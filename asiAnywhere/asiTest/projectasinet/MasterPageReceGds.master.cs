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
public partial class MasterReceGds : System.Web.UI.MasterPage
{
    public MasterReceGds()
    {
        //
        // TODO: Add constructor logic here
        //
    }

    protected void Page_Load(object sender, EventArgs e)
    {
        string fname = this.Page.GetType().Name.ToString();


        if (fname == "recelist_rcpt_aspx" || fname == "delete_list_rcpt.aspx" )
        {

            lilistrcpt.Attributes.Add("class", "selected");
        }
        else if (fname == "receview_rcpt_aspx" || fname == "delete_view_rcpt_aspx")
        {
            liviewrcpt.Attributes.Add("class", "selected");
        }
        else if (fname == "rece_set_part_aspx")
        {
            lisetitem.Attributes.Add("class", "selected");
        }
        else 
        {
            liviewsetitem.Attributes.Add("class", "selected");
        }

        if (!Page.IsPostBack)
        {
            
            if (Session["User"] != null)
            {
                UserClass UserLogin = (UserClass)Session["User"];
                lblUser.Text = UserLogin.UserName;


                string vUserId = UserLogin.UserName;
                string vPage = "recelist_rcpt.aspx";
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
                    lnkrecelist_rcpt.Visible = true;
                    //recelist_rcpt.Visible = true;

                }

                if (vCanRun == false)
                {
                    lnkrecelist_rcpt.Visible = false;
                    //recelist_rcpt.Visible = false;

                }


                

            }
            if (Convert.ToString(Session["master_rece_delete_rece_list"]) == "rece")
            {
                liviewrcpt.Visible = false;
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

    protected void recelist_rcpt_Click(object sender, EventArgs e)
    {
        if (Convert.ToString(Session["master_rece_delete_rece_list"]) == "rece")
        {
            Response.Redirect("recelist_rcpt.aspx");
        }
        else if (Convert.ToString(Session["master_rece_delete_rece_list"]) == "delete")
        {
            Response.Redirect("delete_list_rcpt.aspx");
        }
        else
        {
            string sLoginURL = ConfigurationManager.AppSettings["LoginFile"];
            Response.Redirect(sLoginURL);
        }

        
    }

    protected void receview_rcpt_Click(object sender, EventArgs e)
    {
        
       

        if (Convert.ToString(Session["master_rece_delete_rece_list"]) == "rece")
        {
            Response.Redirect("receview_rcpt.aspx");
        }
        else if (Convert.ToString(Session["master_rece_delete_rece_list"]) == "delete")
        {
            Response.Redirect("delete_view_rcpt.aspx");
        }
        else
        {
            string sLoginURL = ConfigurationManager.AppSettings["LoginFile"];
            Response.Redirect(sLoginURL);
        }
    }
   
    
    
    protected void rece_listitem_click(object sender, EventArgs e)
    {
       Response.Redirect("rece_set_part.aspx");
    }

    
    protected void img_btn_add_click(object sender, ImageClickEventArgs e)
    {         

       if (Convert.ToString(Session["master_rece_delete_rece_list"]) == "rece")
       {
           Session["add_rcpt_list_buton"] = "Additem";
           Response.Redirect("recelist_rcpt.aspx");
       }
       else if (Convert.ToString(Session["master_rece_delete_rece_list"]) == "delete")
       {
           Session["add_rcpt_list_buton"] = "Additem";
           Response.Redirect("delete_view_rcpt.aspx");
       }
       else
       {
           string sLoginURL = ConfigurationManager.AppSettings["LoginFile"];
           Response.Redirect(sLoginURL);
       }

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

    protected void rece_viewset_click(object sender, EventArgs e)
    {
        Response.Redirect("rece_viewset_part.aspx");
    }

    
}