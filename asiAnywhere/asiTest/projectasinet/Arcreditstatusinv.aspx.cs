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
using System.Data.SqlClient;
using System.Linq;

public partial class Arcreditstatusinv : System.Web.UI.Page
{
    protected void Page_Load(object sender, EventArgs e)
    {
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];

        lbl_page.Text = "Credit Status";
        if (!Page.IsPostBack)
        {
            
            ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;

            if (Session["User"] != null)
            {
                lblUser.Text = UserLogin.UserName;

                string vUserId = UserLogin.UserName;
                string vPage = "Arcreditstatusinv.aspx";
                string aUsers = null;
                string PrmComp = null;
                bool vCanCreate = false;
                bool vCanRun = false;
                bool vCanUpdate = false;
                bool vCanDelete = false;

                func1 f1 = new func1();
                //Response.Write(Page);
                f1.CheckProgramPermissions(vPage, vUserId, ref  vCanCreate, ref  vCanRun, ref  vCanUpdate, ref  vCanDelete, ref  PrmComp, ref  aUsers);
               
                if (vCanRun == false)
                {
                    Response.Write("<script>alert('Sorry! You don't have permission to access this page');</script>");
                    Response.Write("<script>window.location.href = 'login.aspx';</script>");
                }

                string vUserIdArinvoice = UserLogin.UserName;
                string vPageArinvoice = "Arinvoice.aspx";
                string aUsersArinvoice = null;
                string PrmCompArinvoice = null;
                bool vCanCreateArinvoice = false;
                bool vCanRunArinvoice = false;
                bool vCanUpdateArinvoice = false;
                bool vCanDeleteArinvoice = false;

                func1 f1Arinvoice = new func1();
                //Response.Write(Page);
                f1Arinvoice.CheckProgramPermissions(vPageArinvoice, vUserIdArinvoice, ref  vCanCreateArinvoice, ref  vCanRunArinvoice, ref  vCanUpdateArinvoice, ref  vCanDeleteArinvoice, ref  PrmCompArinvoice, ref  aUsersArinvoice);

                lblComp.Text = PrmCompArinvoice;
                //Response.Write(PrmCompArinvoice);
                if (vCanRunArinvoice == true)
                {
                    lnkbrowsinvoice.Visible = true;

                }

                if (vCanRunArinvoice == false)
                {
                    lnkbrowsinvoice.Visible = false;
                    
                }

                string vUserIdArviewinvoice = UserLogin.UserName;
                string vPageArviewinvoice = "Arviewinvoice.aspx";
                string aUsersArviewinvoice = null;
                string PrmCompArviewinvoice = null;
                bool vCanCreateArviewinvoice = false;
                bool vCanRunArviewinvoice = false;
                bool vCanUpdateArviewinvoice = false;
                bool vCanDeleteArviewinvoice = false;

                func1 f1Arviewinvoice = new func1();
                //Response.Write(Page);
                f1Arviewinvoice.CheckProgramPermissions(vPageArviewinvoice, vUserIdArviewinvoice, ref  vCanCreateArviewinvoice, ref  vCanRunArviewinvoice, ref  vCanUpdateArviewinvoice, ref  vCanDeleteArviewinvoice, ref  PrmCompArviewinvoice, ref  aUsersArviewinvoice);

                lblComp.Text = PrmCompArviewinvoice;
                //Response.Write(vCanRun);
                if (vCanRunArviewinvoice == true)
                {
                    lnkviewinvoice.Visible = true;

                }

                if (vCanRunArviewinvoice == false)
                {
                    lnkviewinvoice.Visible = false;
                    
                }
                string vUserIdArbol = UserLogin.UserName;
                string vPageArbol = "Arbol.aspx";
                string aUsersArbol = null;
                string PrmCompArbol = null;
                bool vCanCreateArbol = false;
                bool vCanRunArbol = false;
                bool vCanUpdateArbol = false;
                bool vCanDeleteArbol = false;

                func1 f1Arbol = new func1();
                //Response.Write(Page);
                f1Arbol.CheckProgramPermissions(vPageArbol, vUserIdArbol, ref  vCanCreateArbol, ref  vCanRunArbol, ref  vCanUpdateArbol, ref  vCanDeleteArbol, ref  PrmCompArbol, ref  aUsersArbol);

                lblComp.Text = PrmCompArbol;
                //Response.Write(vCanRun);
                if (vCanRunArbol == true)
                {
                    lnkbol.Visible = true;

                }

                if (vCanRunArbol == false)
                {
                    lnkbol.Visible = false;
                    
                }

            }
        }

        SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
        try
        {
            conn.Open();

            string cmd = "select * from button_maintain where parent = 'Arinvoice.aspx' ";
            SqlDataAdapter da = new SqlDataAdapter(cmd, conn);
            DataSet ds = new DataSet();
            da.Fill(ds);

            if (ds.Tables[0].Rows.Count == 0)
            {
                SqlCommand cmd_insert = new SqlCommand("insert into button_maintain (parent, name, btn1, btn2, btn3, btn4, btn5,chk1,chk2,chk3,chk4,chk5,user1,user2,user3,user4,user5,user6,user7,user8,user9,user10,user11,user12,user13,user14,user15) values ('Arinvoice.aspx','AR Invoice Inquiry','Credit Status','Bol','Print Bol','Signed Bol','Print Invoice','True','True','True','True','True','" + UserLogin.UserName + "','" + UserLogin.UserName + "','" + UserLogin.UserName + "','" + UserLogin.UserName + "','" + UserLogin.UserName + "','" + UserLogin.UserName + "','" + UserLogin.UserName + "','" + UserLogin.UserName + "','" + UserLogin.UserName + "','" + UserLogin.UserName + "','" + UserLogin.UserName + "','" + UserLogin.UserName + "','" + UserLogin.UserName + "','" + UserLogin.UserName + "','" + UserLogin.UserName + "')", conn);
                cmd_insert.ExecuteNonQuery();
            }

            foreach (DataRow dr in ds.Tables[0].Rows)
            {
                string[] ss1 = dr["user1"].ToString().Split(',');
                string[] ss2 = dr["user2"].ToString().Split(',');
                string[] ss3 = dr["user3"].ToString().Split(',');
                string[] ss4 = dr["user4"].ToString().Split(',');
                string[] ss5 = dr["user5"].ToString().Split(',');


                if (ss1.Contains(UserLogin.UserName) || ss1.Contains("*"))
                {                   
                    if (dr["chk1"].ToString() == "False")
                        liCreditStatus.Attributes.Add("style", "display:none");
                }
                if (ss2.Contains(UserLogin.UserName) || ss2.Contains("*"))
                {                    
                    if (dr["chk2"].ToString() == "False")
                        liBol.Attributes.Add("style", "display:none");
                }               



            }
            conn.Close();
        }
        catch { conn.Close(); }

    }
    protected void lnkbrowsinvoice_Click(object sender, EventArgs e)
    {
        Response.Redirect("Arinvoice.aspx");
    }

    protected void lnkviewinvoice_Click(object sender, EventArgs e)
    {
        Response.Redirect("Arviewinvoice.aspx");
    }
    protected void lnkbol_Click(object sender, EventArgs e)
    {
        Response.Redirect("Arbol.aspx");
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
