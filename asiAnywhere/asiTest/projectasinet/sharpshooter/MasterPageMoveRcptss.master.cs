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

namespace projectasinet.sharpshooter
{
    public partial class MasterMoveRcptss : System.Web.UI.MasterPage
    {
        public MasterMoveRcptss()
        {
            //
            // TODO: Add constructor logic here
            //
        }

        protected void Page_Load(object sender, EventArgs e)
        {
            if (!Page.IsPostBack)
            {

                if (Session["User"] != null)
                {
                    UserClass UserLogin = (UserClass)Session["User"];
                    lblUser.Text = UserLogin.UserName;


                    string vUserId = UserLogin.UserName;
                    string vPage = "movelist_rcpt.aspx";
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
                        lnkmovelist_rcpt.Visible = true;
                        movelist_rcpt.Visible = true;

                    }

                    if (vCanRun == false)
                    {
                        lnkmovelist_rcpt.Visible = false;
                        movelist_rcpt.Visible = false;

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
            string sLoginURL = ConfigurationManager.AppSettings["LoginFileSs"];
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

        protected void recelist_rcpt_Click(object sender, ImageClickEventArgs e)
        {
            Response.Redirect("movelist_rcptss.aspx");
        }

        protected void receview_rcpt_Click(object sender, ImageClickEventArgs e)
        {

            Response.Redirect("moveview_rcptss.aspx");
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
}