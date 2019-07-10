
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

public partial class corr_print_gsm : System.Web.UI.Page
{

    protected void Page_Load(object sender, System.EventArgs e)
    {
                
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        FormView1.ChangeMode(FormViewMode.Edit);
       
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
            if (aUsers == "external")
            {
                if (!Page.IsPostBack)
                {
                    try
                    {
                        
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
        TextBox brokencomm = (TextBox)FormView1.FindControl("ld_gsa_fmTextBox");
        TextBox unitcount = (TextBox)FormView1.FindControl("ld_gsa_war_u_cTextBox");
        TextBox palletcount = (TextBox)FormView1.FindControl("ld_gsa_war_cntTextBox");
        TextBox totalunit = (TextBox)FormView1.FindControl("ld_gsa_war_uniTextBox");
        TextBox unitperpallet = (TextBox)FormView1.FindControl("ld_gsa_war_u_pTextBox");
        TextBox totalpallet = (TextBox)FormView1.FindControl("ld_gsa_war_palTextBox");
        TextBox costperpallet = (TextBox)FormView1.FindControl("ld_gsa_war_amtTextBox");
        TextBox pallethandcharge = (TextBox)FormView1.FindControl("ld_gsa_war_hdlTextBox");
        TextBox palletmonth = (TextBox)FormView1.FindControl("ld_gsa_war_perTextBox");
        TextBox totalcharge = (TextBox)FormView1.FindControl("ld_gsa_war_totTextBox");

        ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
        ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "select";
        ObjectDataSource1.SelectParameters["prmIpQty"].DefaultValue = qty.Text.Trim();
        ObjectDataSource1.SelectParameters["prmIpRels"].DefaultValue = "";
        ObjectDataSource1.SelectParameters["prmBoard"].DefaultValue = brd.Text.Trim();
        ObjectDataSource1.SelectParameters["prmMaterial"].DefaultValue = mat.Text.Trim();
        ObjectDataSource1.SelectParameters["prmLabor"].DefaultValue = labor.Text.Trim();
        ObjectDataSource1.SelectParameters["prmWHMark"].DefaultValue = wherehousemark.Text.Trim();
        ObjectDataSource1.SelectParameters["prmBrComm"].DefaultValue = brokencomm.Text.Trim();
        ObjectDataSource1.SelectParameters["prmUnCount"].DefaultValue = unitcount.Text.Trim();
        ObjectDataSource1.SelectParameters["prmPallCount"].DefaultValue = palletcount.Text.Trim();
        ObjectDataSource1.SelectParameters["prmTotUnit"].DefaultValue = totalunit.Text.Trim();
        ObjectDataSource1.SelectParameters["prmUnitprPal"].DefaultValue = unitperpallet.Text.Trim();
        ObjectDataSource1.SelectParameters["prmPaHandCh"].DefaultValue = pallethandcharge.Text.Trim();
        ObjectDataSource1.SelectParameters["prmPallDelMon"].DefaultValue = palletmonth.Text.Trim();
        ObjectDataSource1.SelectParameters["prmTotCharge"].DefaultValue = totalcharge.Text.Trim();
        ObjectDataSource1.SelectParameters["prmCostPerPall"].DefaultValue = costperpallet.Text.Trim();
        ObjectDataSource1.SelectParameters["prmTotPall"].DefaultValue = totalpallet.Text.Trim();

        ObjectDataSource1.SelectParameters["prmGsaMat"].DefaultValue = Convert.ToString(Session["estimate_analysis_gsa_mat"]);
        ObjectDataSource1.SelectParameters["prmGsaLab"].DefaultValue = Convert.ToString(Session["estimate_analysis_gsa_lab"]);
        ObjectDataSource1.SelectParameters["prmGsaWar"].DefaultValue = Convert.ToString(Session["estimate_analysis_gsa_war"]);
        ObjectDataSource1.SelectParameters["prmGsaFm"].DefaultValue = Convert.ToString(Session["estimate_analysis_gsa_fm"]);
        ObjectDataSource1.SelectParameters["prmGsaMonth"].DefaultValue = Convert.ToString(Session["estimate_analysis_gsa_month"]);


        SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
        try
        {
            conn.Open();

            string cmd = "select * from corr_print_maintenance  where user_name = '" + UserLogin.UserName + "' and est = '" + Convert.ToString(Session["order_corrugated_est"]) + "' and seq_no = '" + Request.QueryString["seq"] + "' ";
            SqlDataAdapter da = new SqlDataAdapter(cmd, conn);
            DataSet ds = new DataSet();
            da.Fill(ds);
            conn.Close();

            ObjectDataSource1.SelectParameters["prmSeq"].DefaultValue = Request.QueryString["seq"];
            ObjectDataSource1.SelectParameters["prmQty"].DefaultValue = ds.Tables[0].Rows[0][3].ToString();
            ObjectDataSource1.SelectParameters["prmGetqty"].DefaultValue = ds.Tables[0].Rows[0][4].ToString();
            ObjectDataSource1.SelectParameters["prmMatchup"].DefaultValue = ds.Tables[0].Rows[0][6].ToString();
            ObjectDataSource1.SelectParameters["prmDoGsa"].DefaultValue = ds.Tables[0].Rows[0][7].ToString();
            ObjectDataSource1.SelectParameters["prmDoMr"].DefaultValue = ds.Tables[0].Rows[0][8].ToString();
            ObjectDataSource1.SelectParameters["prmDoSpeed"].DefaultValue = ds.Tables[0].Rows[0][9].ToString();
            ObjectDataSource1.SelectParameters["prmDropRc"].DefaultValue = ds.Tables[0].Rows[0][10].ToString();
            ObjectDataSource1.SelectParameters["prmInkAlForms"].DefaultValue = ds.Tables[0].Rows[0][11].ToString();



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
         TextBox brokencomm = (TextBox)FormView1.FindControl("ld_gsa_fmTextBox");
         TextBox unitcount = (TextBox)FormView1.FindControl("ld_gsa_war_u_cTextBox");
         TextBox palletcount = (TextBox)FormView1.FindControl("ld_gsa_war_cntTextBox");
         TextBox totalunit = (TextBox)FormView1.FindControl("ld_gsa_war_uniTextBox");
         TextBox unitperpallet = (TextBox)FormView1.FindControl("ld_gsa_war_u_pTextBox");
         TextBox totalpallet = (TextBox)FormView1.FindControl("ld_gsa_war_palTextBox");
         TextBox costperpallet = (TextBox)FormView1.FindControl("ld_gsa_war_amtTextBox");
         TextBox pallethandcharge = (TextBox)FormView1.FindControl("ld_gsa_war_hdlTextBox");
         TextBox palletmonth = (TextBox)FormView1.FindControl("ld_gsa_war_perTextBox");
         TextBox totalcharge = (TextBox)FormView1.FindControl("ld_gsa_war_totTextBox");
        

         ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
         ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "update";
         ObjectDataSource1.SelectParameters["prmIpQty"].DefaultValue = qty.Text.Trim();
         ObjectDataSource1.SelectParameters["prmIpRels"].DefaultValue = "";         
         ObjectDataSource1.SelectParameters["prmBoard"].DefaultValue = brd.Text.Trim();
         ObjectDataSource1.SelectParameters["prmMaterial"].DefaultValue = mat.Text.Trim();
         ObjectDataSource1.SelectParameters["prmLabor"].DefaultValue = labor.Text.Trim();
         ObjectDataSource1.SelectParameters["prmWHMark"].DefaultValue = wherehousemark.Text.Trim();
         ObjectDataSource1.SelectParameters["prmBrComm"].DefaultValue = brokencomm.Text.Trim();
         ObjectDataSource1.SelectParameters["prmUnCount"].DefaultValue = unitcount.Text.Trim();
         ObjectDataSource1.SelectParameters["prmPallCount"].DefaultValue = palletcount.Text.Trim();
         ObjectDataSource1.SelectParameters["prmTotUnit"].DefaultValue = totalunit.Text.Trim();
         ObjectDataSource1.SelectParameters["prmUnitprPal"].DefaultValue = unitperpallet.Text.Trim();
         ObjectDataSource1.SelectParameters["prmPaHandCh"].DefaultValue = pallethandcharge.Text.Trim();
         ObjectDataSource1.SelectParameters["prmPallDelMon"].DefaultValue = palletmonth.Text.Trim();
         ObjectDataSource1.SelectParameters["prmTotCharge"].DefaultValue = totalcharge.Text.Trim();
         ObjectDataSource1.SelectParameters["prmCostPerPall"].DefaultValue = costperpallet.Text.Trim();
         ObjectDataSource1.SelectParameters["prmTotPall"].DefaultValue = totalpallet.Text.Trim();
         ObjectDataSource1.SelectParameters["prmVendor"].DefaultValue = Convert.ToString(Session["estimate_analysis_vendor_no"]);

          
        SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
        try
        {
            conn.Open();

            string cmd = "select * from corr_print_maintenance  where user_name = '" + UserLogin.UserName + "' and est = '" + Convert.ToString(Session["order_corrugated_est"]) + "' and seq_no = '" + Request.QueryString["seq"] + "' ";
            SqlDataAdapter da = new SqlDataAdapter(cmd, conn);
            DataSet ds = new DataSet();
            da.Fill(ds);
            conn.Close();

            ObjectDataSource1.SelectParameters["prmSeq"].DefaultValue = Request.QueryString["seq"];
            ObjectDataSource1.SelectParameters["prmQty"].DefaultValue = ds.Tables[0].Rows[0][3].ToString();
            ObjectDataSource1.SelectParameters["prmGetqty"].DefaultValue = ds.Tables[0].Rows[0][4].ToString();
            ObjectDataSource1.SelectParameters["prmMatchup"].DefaultValue = ds.Tables[0].Rows[0][6].ToString();
            ObjectDataSource1.SelectParameters["prmDoGsa"].DefaultValue = ds.Tables[0].Rows[0][7].ToString();
            ObjectDataSource1.SelectParameters["prmDoMr"].DefaultValue = ds.Tables[0].Rows[0][8].ToString();
            ObjectDataSource1.SelectParameters["prmDoSpeed"].DefaultValue = ds.Tables[0].Rows[0][9].ToString();
            ObjectDataSource1.SelectParameters["prmDropRc"].DefaultValue = ds.Tables[0].Rows[0][10].ToString();
            ObjectDataSource1.SelectParameters["prmInkAlForms"].DefaultValue = ds.Tables[0].Rows[0][11].ToString();



        }
        catch { }


        try
        {
            conn.Open();
            string cmd2 = "select max(seq_no) from corr_print_maintenance  where user_name = '" + UserLogin.UserName + "' and est = '" + Convert.ToString(Session["order_corrugated_est"]) + "'  ";
            SqlDataAdapter da2 = new SqlDataAdapter(cmd2, conn);
            DataSet ds2 = new DataSet();
            da2.Fill(ds2);

            if (Convert.ToInt32(Request.QueryString["seq"]) < Convert.ToInt32(ds2.Tables[0].Rows[0][0].ToString()))
            {
                int seqno = Convert.ToInt32(Request.QueryString["seq"]) + 1;
                Response.Write("<script>location.href='corr_print_gsm.aspx?seq=" + seqno + "'</script>");
            }
            else
            {
                
                Response.Write("<script>javascript:self.close();window.opener.location.reload();</script>");
            }

        }
        catch { }
    }

    protected void formview1_databound(object sender, EventArgs e)
    {
        if (FormView1.CurrentMode == FormViewMode.Edit)
        {
            TextBox wherehousemark = (TextBox)FormView1.FindControl("ld_gsa_warTextBox");
            TextBox brokencomm = (TextBox)FormView1.FindControl("ld_gsa_fmTextBox");
            TextBox unitcount = (TextBox)FormView1.FindControl("ld_gsa_war_u_cTextBox");
            TextBox palletcount = (TextBox)FormView1.FindControl("ld_gsa_war_cntTextBox");
            TextBox totalunit = (TextBox)FormView1.FindControl("ld_gsa_war_uniTextBox");
            TextBox unitperpallet = (TextBox)FormView1.FindControl("ld_gsa_war_u_pTextBox");
            TextBox totalpallet = (TextBox)FormView1.FindControl("ld_gsa_war_palTextBox");
            TextBox costperpallet = (TextBox)FormView1.FindControl("ld_gsa_war_amtTextBox");
            try
            {
                wherehousemark.Enabled = false;
                unitcount.Enabled = false;
                palletcount.Enabled = false;
                totalunit.Enabled = false;
            }
            catch { }
            
        }
    }
    
}
