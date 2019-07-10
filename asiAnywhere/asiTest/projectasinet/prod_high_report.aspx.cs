
#region " using "
using System;
using System.Data;
using System.Web.UI.WebControls;
using System.Collections;
using System.Configuration;
using System.Threading;
using System.Globalization;
using System.Data.SqlClient;
using System.Web.UI.HtmlControls;
using System.Web.UI.WebControls.WebParts;
using System.Web;
#endregion

public partial class prod_high_report : System.Web.UI.Page
{
    string[] arrmachine = new string[15];
    public prod_high_report()
    {
        for (int p = 0; p < 15; p++)
        {
            arrmachine[p] = "";
        }
    }
   
    protected void Page_Load(object sender, System.EventArgs e)
    {

        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
       

        if (Session["User"] != null)
        {
            string vUserId = UserLogin.UserName;
            string vPage = "prop_high_report.aspx";
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
                                       
                            DateTextBox.Text = Convert.ToString( System.DateTime.Today.Date.ToShortDateString());
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
        ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
        ObjectDataSource1.SelectParameters["prmComp"].DefaultValue = CompanyTextBox.Text.Trim();
        

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
        int j = 0;
        for (int i = 0; i < GridView1.Rows.Count; i++)
        {
            GridViewRow row = GridView1.Rows[i];
            bool ischeck = ((CheckBox)row.FindControl("chk1")).Checked;
            if (ischeck)
            {
                if (j > 14)
                {                    
                    HttpContext.Current.Response.Write("<script>alert('More than 15 Machines are Selected')</script>");
                    return;
                }
                
                arrmachine[j] = GridView1.Rows[i].Cells[1].Text;
                j = j + 1;                
                
            }

        }


        ObjectDataSource2.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
        ObjectDataSource2.SelectParameters["prmAction"].DefaultValue = "RunReport";
        ObjectDataSource2.SelectParameters["prmDate"].DefaultValue = DateTextBox.Text.Trim(); ;
        ObjectDataSource2.SelectParameters["prmComp"].DefaultValue = CompanyTextBox.Text.Trim();
        ObjectDataSource2.SelectParameters["prmMachine1"].DefaultValue = arrmachine[0].ToString();
        ObjectDataSource2.SelectParameters["prmMachine2"].DefaultValue = arrmachine[1].ToString();
        ObjectDataSource2.SelectParameters["prmMachine3"].DefaultValue = arrmachine[2].ToString();
        ObjectDataSource2.SelectParameters["prmMachine4"].DefaultValue = arrmachine[3].ToString();
        ObjectDataSource2.SelectParameters["prmMachine5"].DefaultValue = arrmachine[4].ToString();
        ObjectDataSource2.SelectParameters["prmMachine6"].DefaultValue = arrmachine[5].ToString();
        ObjectDataSource2.SelectParameters["prmMachine7"].DefaultValue = arrmachine[6].ToString();
        ObjectDataSource2.SelectParameters["prmMachine8"].DefaultValue = arrmachine[7].ToString();
        ObjectDataSource2.SelectParameters["prmMachine9"].DefaultValue = arrmachine[8].ToString();
        ObjectDataSource2.SelectParameters["prmMachine10"].DefaultValue = arrmachine[9].ToString();
        ObjectDataSource2.SelectParameters["prmMachine11"].DefaultValue = arrmachine[10].ToString();
        ObjectDataSource2.SelectParameters["prmMachine12"].DefaultValue = arrmachine[11].ToString();
        ObjectDataSource2.SelectParameters["prmMachine13"].DefaultValue = arrmachine[12].ToString();
        ObjectDataSource2.SelectParameters["prmMachine14"].DefaultValue = arrmachine[13].ToString();
        ObjectDataSource2.SelectParameters["prmMachine15"].DefaultValue = arrmachine[14].ToString();

       
         try
         {
        OutputLabel.Visible = true;
        HyperLink1.Visible = true;
       
        Label path = (Label)FormView2.FindControl("vDashFileLabel");       
        HyperLink1.Text = path.Text;
        HyperLink1.NavigateUrl = @"/pdfs/" + path.Text;

        if (path.Text == "")
        {
            Label1.Text = "No Csv Exists";
            //Response.Write("<script>window.location.href= 'prod_high_report.aspx'</script>");
        }
        if (path.Text != "")
        {
            //string path2 = @"/pdfs/" + path.Text;
           // Response.Redirect(path2);
        }
       }
       catch
       {

       }
        
    }
    protected void GridView1_SelectedIndexChanged(object sender, EventArgs e)
    {
        
    }
    protected void company_text_change(object sender, EventArgs e)
    {
       GridView1.DataBind();
       Page_Load(sender, e);
    }
   
}
