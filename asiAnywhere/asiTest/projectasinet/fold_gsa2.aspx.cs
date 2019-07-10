
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

public partial class fold_gsa2 : System.Web.UI.Page
{

    protected void Page_Load(object sender, System.EventArgs e)
    {
                
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        FormView1.ChangeMode(FormViewMode.Edit);
        if (Session["User"] != null)
        {
            string vUserId = UserLogin.UserName;
            string vPage = "fold_print.aspx";
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
            if (aUsers == "external")
            {
                if (!Page.IsPostBack)
                {
                    try
                    {
                        //string UserId = UserLogin.UserName;
                        //string aDefaultCust = null;
                        //string aComp = null;

                        //func1 user = new func1();
                        //user.CheckUserCustomer(aComp, UserId, ref  aDefaultCust);
                        //begincust_TextBox.Text = aDefaultCust;
                        //endcust_TextBox.Text = aDefaultCust;
                    }
                    catch { }

                    

                }
            }
            if (aUsers == "internal")
            {
                if (!Page.IsPostBack)
                {

                }

            }

            if (vCanRun == false)
            {
                Response.Write("<script>alert('Sorry! You don't have permission to access this page');</script>");
                Response.Write("<script>window.location.href = 'login.aspx';</script>");

            }
        }
        
        Label qty = (Label)FormView1.FindControl("ld_qtyTextBox");
        TextBox mat = (TextBox)FormView1.FindControl("ld_gsa_matTextBox");
        TextBox brd = (TextBox)FormView1.FindControl("ld_gsa_brdTextBox");
        TextBox labor = (TextBox)FormView1.FindControl("ld_gsa_labTextBox");
        TextBox wherehousemark = (TextBox)FormView1.FindControl("ld_gsa_warTextBox");
        TextBox foldfg = (TextBox)FormView1.FindControl("foldTextBox");       


        ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
        ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "select";
        ObjectDataSource1.SelectParameters["prmIpQty"].DefaultValue = qty.Text.Trim();
        ObjectDataSource1.SelectParameters["prmIpRels"].DefaultValue = "";
        ObjectDataSource1.SelectParameters["prmBoard"].DefaultValue = brd.Text.Trim();
        ObjectDataSource1.SelectParameters["prmMaterial"].DefaultValue = mat.Text.Trim();
        ObjectDataSource1.SelectParameters["prmLabor"].DefaultValue = labor.Text.Trim();
        ObjectDataSource1.SelectParameters["prmWHMark"].DefaultValue = wherehousemark.Text.Trim();
        ObjectDataSource1.SelectParameters["prmGsafm"].DefaultValue = foldfg.Text.Trim();

        ObjectDataSource1.SelectParameters["prmGsaMat"].DefaultValue = Convert.ToString(Session["fold_analysis1_gsa_mat"]);
        ObjectDataSource1.SelectParameters["prmGsaLab"].DefaultValue = Convert.ToString(Session["fold_analysis1_gsa_lab"]);
        ObjectDataSource1.SelectParameters["prmGsaWar"].DefaultValue = Convert.ToString(Session["fold_analysis1_gsa_war"]);
        ObjectDataSource1.SelectParameters["prmGsaMonth"].DefaultValue = Convert.ToString(Session["fold_analysis1_gsa_month"]);
        


        SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
        try
        {
            conn.Open();

            string cmd = "select * from corr_print_maintenance  where user_name = '" + UserLogin.UserName + "' and est = '" + Convert.ToString(Session["order_folding_est"]) + "' and seq_no = '" + Request.QueryString["seq"] + "' ";
            SqlDataAdapter da = new SqlDataAdapter(cmd, conn);
            DataSet ds = new DataSet();
            da.Fill(ds);
            conn.Close();

            ObjectDataSource1.SelectParameters["prmSeq"].DefaultValue = Request.QueryString["seq"];
            ObjectDataSource1.SelectParameters["prmQty"].DefaultValue = ds.Tables[0].Rows[0][3].ToString();
            ObjectDataSource1.SelectParameters["prmGetqty"].DefaultValue = ds.Tables[0].Rows[0][4].ToString();
           
            ObjectDataSource1.SelectParameters["prmDoGsa"].DefaultValue = ds.Tables[0].Rows[0][7].ToString();
            ObjectDataSource1.SelectParameters["prmDoMr"].DefaultValue = ds.Tables[0].Rows[0][8].ToString();
            ObjectDataSource1.SelectParameters["prmDoSpeed"].DefaultValue = ds.Tables[0].Rows[0][9].ToString();
          

        }
        catch { }


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
    
    

    protected void PurgeQtyClick(object sender, EventArgs e)
    {
        Page.ClientScript.RegisterStartupScript
                            (this.GetType(), "displayQtyScrDiv", "displayQtyScrDiv();", true);
    }
    protected void AppendQtyClick(object sender, EventArgs e)
    {
        Page.ClientScript.RegisterStartupScript
                            (this.GetType(), "displayQtyScrDiv", "displayQtyScrDiv();", true);
    }
    protected void updatebutton_click(object sender, EventArgs e)
    {
         UserClass UserLogin = (UserClass)Session["User"];
         Label qty = (Label)FormView1.FindControl("ld_qtyTextBox");
         TextBox mat = (TextBox)FormView1.FindControl("ld_gsa_matTextBox");
         TextBox brd = (TextBox)FormView1.FindControl("ld_gsa_brdTextBox");
         TextBox labor = (TextBox)FormView1.FindControl("ld_gsa_labTextBox");
         TextBox wherehousemark = (TextBox)FormView1.FindControl("ld_gsa_warTextBox");
         TextBox foldfg = (TextBox)FormView1.FindControl("foldTextBox");        
        

         ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
         ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "update";
         ObjectDataSource1.SelectParameters["prmIpQty"].DefaultValue = qty.Text.Trim();
         ObjectDataSource1.SelectParameters["prmIpRels"].DefaultValue = "";         
         ObjectDataSource1.SelectParameters["prmBoard"].DefaultValue = brd.Text.Trim();
         ObjectDataSource1.SelectParameters["prmMaterial"].DefaultValue = mat.Text.Trim();
         ObjectDataSource1.SelectParameters["prmLabor"].DefaultValue = labor.Text.Trim();
         ObjectDataSource1.SelectParameters["prmWHMark"].DefaultValue = wherehousemark.Text.Trim();
         ObjectDataSource1.SelectParameters["prmGsafm"].DefaultValue = foldfg.Text.Trim();
         ObjectDataSource1.SelectParameters["prmVendor"].DefaultValue = Convert.ToString(Session["fold_analysis_1_vendor_no"]);
         
          
        SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
        try
        {
            conn.Open();

            string cmd = "select * from corr_print_maintenance  where user_name = '" + UserLogin.UserName + "' and est = '" + Convert.ToString(Session["order_folding_est"]) + "' and seq_no = '" + Request.QueryString["seq"] + "' ";
            SqlDataAdapter da = new SqlDataAdapter(cmd, conn);
            DataSet ds = new DataSet();
            da.Fill(ds);
            conn.Close();

            ObjectDataSource1.SelectParameters["prmSeq"].DefaultValue = Request.QueryString["seq"];
            ObjectDataSource1.SelectParameters["prmQty"].DefaultValue = ds.Tables[0].Rows[0][3].ToString();
            ObjectDataSource1.SelectParameters["prmGetqty"].DefaultValue = ds.Tables[0].Rows[0][4].ToString();
          
            ObjectDataSource1.SelectParameters["prmDoGsa"].DefaultValue = ds.Tables[0].Rows[0][7].ToString();
            ObjectDataSource1.SelectParameters["prmDoMr"].DefaultValue = ds.Tables[0].Rows[0][8].ToString();
            ObjectDataSource1.SelectParameters["prmDoSpeed"].DefaultValue = ds.Tables[0].Rows[0][9].ToString();
           

        }
        catch { }

        Response.Write("<script>javascript:self.close();window.opener.location.reload();</script>");            
          
    }

    protected void formview1_databound(object sender, EventArgs e)
    {
        
    }
}
