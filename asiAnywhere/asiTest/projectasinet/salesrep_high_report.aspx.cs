
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

public partial class salesrep_high_report : System.Web.UI.Page
{

   
    protected void Page_Load(object sender, System.EventArgs e)
    {

        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
        if (Session["User"] != null)
        {
            string vUserId = UserLogin.UserName;
            string vPage = "salesrep_high_report.aspx";
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

            lblUser.Text = UserLogin.UserName;

            Session["Customers_Company"] = labelcompany.Text;
             if (!Page.IsPostBack)
                {
                    OutputLabel.Visible = false;
                    HyperLink1.Visible = false;

                    DateTextBox.Text = DateTime.Today.ToShortDateString();
                    CompanyTextBox.Text = PrmComp;                                                                     
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

   protected void submitbutton_click(object sender, EventArgs e)
    {
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];

        ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
        ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "RunReport";
        ObjectDataSource1.SelectParameters["prmDate"].DefaultValue = DateTextBox.Text.Trim(); ;
        ObjectDataSource1.SelectParameters["prmComp"].DefaultValue = CompanyTextBox.Text.Trim();      

        try
        {
            Label path = (Label)FormView1.FindControl("vSalesRepFileLabel");


            OutputLabel.Visible = true;
            HyperLink1.Visible = true;
            
            HyperLink1.Text = path.Text;
            HyperLink1.NavigateUrl = @"/pdfs/" + path.Text;

            if (path.Text == "")
            {
                Label1.Text = "No Csv Exists";
                /*Response.Write("<script>window.location.href='salesrep_high_report.aspx'</script>");*/
            }

            if (path.Text != "")
                {     
                    //string path2 = @"/pdfs/" + path.Text;
                    //Response.Redirect(path2);                                         
                }
        }
        catch
        {

        }

    }   
   
}
