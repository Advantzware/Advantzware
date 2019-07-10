
#region " using "
using System;
using System.Data;
using System.Web.UI.WebControls;
using System.Collections;
using System.Configuration;
using System.Threading;
using System.Globalization;
using System.Data.SqlClient;
#endregion

public partial class fold_estimate_probeit : System.Web.UI.Page
{
    protected void Page_Load(object sender, System.EventArgs e)
    {
       
              
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        if (Session["User"] != null)
        {
            string vUserId = UserLogin.UserName;
            string vPage = "corr_print.aspx";
            string aUsers = null;
            string PrmComp = null;
            bool vCanCreate = false;
            bool vCanRun = false;
            bool vCanUpdate = false;
            bool vCanDelete = false;

            func1 f1 = new func1();
            //Response.Write(Page);
            f1.CheckProgramPermissions(vPage, vUserId, ref  vCanCreate, ref  vCanRun, ref  vCanUpdate, ref  vCanDelete, ref  PrmComp, ref  aUsers);

            Session["alpha_login"] = PrmComp;            

            if (vCanRun == false)
            {
                Response.Write("<script>alert('Sorry! You don't have permission to access this page');</script>");
                Response.Write("<script>window.location.href = 'login.aspx';</script>");

            }
        }

        if (!(Page.IsPostBack))
        {
            GridView1.SelectedIndex = 0;
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
    protected void SubmitButton_Click(object sender, EventArgs e)
    {
    }
    protected void FormView1_PreRender(object sender, EventArgs e)
    {
       
    }

    protected void FormView1_DataBound(object sender, EventArgs e)
    {
        
    }  
          
    protected void Gridview1_Sorting(object sender, GridViewSortEventArgs e)
    {

    }

    protected void updateItem(object sender, EventArgs e)
    {
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        TextBox sellprice   = (TextBox)FormView1.FindControl("vSellPriceTextBox");
        TextBox oldsellprc = (TextBox)FormView1.FindControl("TextBox1");
        TextBox partno      = (TextBox)FormView1.FindControl("vPartNoTextBox");

        string estimate = Convert.ToString(Session["order_folding_est"]);
        Int32 lineno = Convert.ToInt32(Session["fold_est_line"]);

        if (oldsellprc.Text != sellprice.Text)
        {
            Corrugated corrugated = new Corrugated();
            corrugated.SelectFoldEstimateProbeit(UserLogin.UserName, "updateitem", estimate, lineno, Convert.ToDecimal(sellprice.Text), partno.Text);

            FormView1.DataBind();
            FormView1.ChangeMode(FormViewMode.ReadOnly);

            Response.Write("<script>window.location.href='fold_estimate_probeit.aspx'</script>");
        }
    }
}
