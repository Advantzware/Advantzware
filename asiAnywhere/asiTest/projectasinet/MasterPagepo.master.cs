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
public partial class MasterPagePo : System.Web.UI.MasterPage
{
    public MasterPagePo()
    {
        //
        // TODO: Add constructor logic here
        //
    }

    protected void Page_Load(object sender, EventArgs e)
    {
        if (!Page.IsPostBack)
        {
            string fname = this.Page.GetType().Name.ToString();


            if (fname == "brwslist_po_aspx")
            {

                lilist_po.Attributes.Add("class", "selected");
            }
            else if (fname == "view_pord_aspx")
            {
                liview_po.Attributes.Add("class", "selected");
            }
            else if (fname == "brwsitem_po_aspx")
            {
                lilist_item.Attributes.Add("class", "selected");
            }
            else if (fname == "viewitem_po_aspx")
            {
                liview_item.Attributes.Add("class", "selected");
            }
            else 
            {
                liview_box.Attributes.Add("class", "selected");
            }

            if (Session["User"] != null)
            {
                UserClass UserLogin = (UserClass)Session["User"];
                lblUser.Text = UserLogin.UserName;


                string vUserId = UserLogin.UserName;
                string vPage = "list_rfqs.aspx";
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
                    lnklist_po.Visible = true;
                    //list_po.Visible = true;

                }

                if (vCanRun == false)
                {
                    lnklist_po.Visible = false;
                    //list_po.Visible = false;

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

    protected void list_po_Click(object sender, EventArgs e)
    {
        Response.Redirect("Brwslist_po.aspx");
    }

    protected void view_po_Click(object sender, EventArgs e)
    {
        
        Response.Redirect("view_pord.aspx");
    }
   
    
    
    protected void lnk_listitem_click(object sender, EventArgs e)
    {
       Response.Redirect("Brwsitem_po.aspx");
    }
    protected void lnk_viewitem_click(object sender, EventArgs e)
    {
        Response.Redirect("viewitem_po.aspx");
    }
    protected void lnk_box_click(object sender, EventArgs e)
    {
        Response.Redirect("po_box_design.aspx");
    }

    
    protected void img_btn_add_click(object sender, ImageClickEventArgs e)
    {
        Session["add_po_list_buton"] = "add";
        Response.Redirect("view_pord.aspx");

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