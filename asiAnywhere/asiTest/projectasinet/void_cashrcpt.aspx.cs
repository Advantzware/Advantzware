
#region " using "
using System;
using System.Data;
using System.Web.UI.WebControls;
using System.Collections;
using System.Configuration;
using System.Threading;
using System.Globalization;
using System.Data.SqlClient;
using System.Web;
#endregion

public partial class void_cashrcpt : System.Web.UI.Page
{
    protected void Page_PreRender(object sender, EventArgs e)
    {
        //try
        //{
        //    GridView1_SelectedIndex(sender, e);
        //}
        //catch { }
    }

   protected void Page_Load(object sender, System.EventArgs e)
    {

        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        if (Session["User"] != null)
        {
            string vUserId = UserLogin.UserName;
            string vPage = "void_cashrcpt.aspx";
            string aUsers = null;
            string PrmComp = null;
            bool vCanCreate = false;
            bool vCanRun = false;
            bool vCanUpdate = false;
            bool vCanDelete = false;

            func1 f1 = new func1();
            //Response.Write(Page);
            f1.CheckProgramPermissions(vPage, vUserId, ref  vCanCreate, ref  vCanRun, ref  vCanUpdate, ref  vCanDelete, ref  PrmComp, ref  aUsers);

            labelcompany.Text = PrmComp;
            Session["Customers_Company"] = labelcompany.Text;
            lblUser.Text = UserLogin.UserName;
            if (aUsers == "external")
            {


            }
            if (vCanRun == false)
            {
                Response.Write("<script>alert('Sorry! You don't have permission to access this page');</script>");
                Response.Write("<script>window.location.href = 'login.aspx';</script>");

            }
        }

        string sCulture = ConfigurationManager.AppSettings["LCID"];
        if (!String.IsNullOrEmpty(sCulture))
        {
            int nCulture = int.Parse(sCulture);
            System.Threading.Thread.CurrentThread.CurrentCulture = new System.Globalization.CultureInfo(nCulture, false);
        }             

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
        Response.Redirect(sLoginURL);
    }

    protected void Back_tomenu_Click(object sender, EventArgs e)
    {
        Response.Redirect("menu.aspx");
    }

    
    

    protected void btnSearch_Click(object sender, EventArgs e)
    {
                
        UserClass UserLogin = (UserClass)Session["User"];
        try
        {
            account con = new account();
            DataSet ds = new DataSet();
            ds = con.VoidCashReceiptList("getdata", "", UserLogin.UserName, Convert.ToInt32(chk_TextBox.Text.Trim()), cust_TextBox.Text.Trim(), "", "", "", "", "", 0, "", "");

            chknoTextBox.Text = Convert.ToString(ds.Tables[0].Rows[0][0]);
            amtTextBox.Text = Convert.ToString(ds.Tables[0].Rows[0][7]);
            custdtTextBox.Text = Convert.ToString(ds.Tables[0].Rows[0][3]);
            custnoTextBox.Text = Convert.ToString(ds.Tables[0].Rows[0][1]);
            custnameTextBox.Text = Convert.ToString(ds.Tables[0].Rows[0][2]);
            bnkTextBox.Text = Convert.ToString(ds.Tables[0].Rows[0][4]);
            bnknamTextBox.Text = Convert.ToString(ds.Tables[0].Rows[0][5]);
            DropDownList1.ClearSelection();
            DropDownList1.Items.FindByValue(Convert.ToString(ds.Tables[0].Rows[0][6])).Selected = true;
            
        }
        catch {
            HttpContext.Current.Response.Write("<script>alert('" + "Invalid Entry" + "')</script>");
        }
    }

    protected void btnSave_Click(object sender, EventArgs e)
    {
        UserClass UserLogin = (UserClass)Session["User"];

        account con = new account();
        DataSet ds = new DataSet();
        ds = con.VoidCashReceiptList("Save", "", UserLogin.UserName, Convert.ToInt32(chk_TextBox.Text.Trim()), cust_TextBox.Text.Trim(), "", "", "", "", Convert.ToString(DropDownList1.SelectedValue), 0, "", "");  
                

    }
    protected void btnCancel_Click(object sender, EventArgs e)
    {
        chknoTextBox.Text = "";
        amtTextBox.Text = "";
        custdtTextBox.Text = "";
        custnoTextBox.Text = "";
        custnameTextBox.Text = "";
        bnkTextBox.Text = "";
        bnknamTextBox.Text = "" ;
        DropDownList1.ClearSelection();
        chk_TextBox.Focus();
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
